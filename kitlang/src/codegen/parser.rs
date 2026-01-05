use pest::iterators::Pair;

use crate::codegen::types::{BinaryOperator, UnaryOperator};
use crate::error::CompilationError;
use crate::{Rule, parse_error};

use super::ast::{Block, Expr, Function, Include, Literal, Param, Stmt};
use super::frontend::CompileResult;
use super::types::{AssignmentOperator, Type};

use std::path::PathBuf;
use std::str::FromStr;

#[derive(Default, Debug)]
pub struct Parser {
    current_file: Option<PathBuf>,
    source_content: String,
}

impl Parser {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_file(file: PathBuf) -> Self {
        let source_content = std::fs::read_to_string(&file).unwrap_or_else(|_| String::new());

        Self {
            current_file: Some(file),
            source_content,
        }
    }

    /// Gets the line content from a line number. Returns an empty string if the line does not exist
    fn get_line_content(&self, line_num: usize) -> String {
        self.source_content
            .lines()
            .nth(line_num.saturating_sub(1))
            .unwrap_or("")
            .to_string()
    }

    pub fn parse_include(&self, pair: Pair<Rule>) -> Include {
        // include_stmt = { "include" ~ string ~ ("=>" ~ string)? ~ ";" }
        let mut inner = pair.into_inner();
        // The first child is the string literal representing the include path
        let path_literal_pair = inner.next().unwrap();
        let path_str = path_literal_pair.as_str();

        // Strip the quotes from the string literal
        let path = path_str[1..path_str.len() - 1].to_string();
        Include { path }
    }

    pub fn parse_function(&self, pair: Pair<Rule>) -> CompileResult<Function> {
        let mut inner = pair.into_inner();

        // SAFETY: Grammar guarantees function name exists as first child
        let name = inner.next().unwrap().as_str().to_string();

        let mut params: Vec<Param> = Vec::new();
        let mut return_type: Option<Type> = None;
        let mut body = Block { stmts: Vec::new() };

        // consume remaining child nodes and match on their rules
        for node in inner {
            match node.as_rule() {
                Rule::params => {
                    params = self.parse_params(node)?;
                }
                Rule::type_annotation => {
                    return_type = Some(self.parse_type(node)?);
                }
                Rule::block => {
                    body = self.parse_block(node)?;
                }
                _ => {
                    // skip punctuation/other tokens
                }
            }
        }

        Ok(Function {
            name,
            params,
            return_type,
            body,
        })
    }

    fn parse_params(&self, pair: Pair<Rule>) -> CompileResult<Vec<Param>> {
        // param_list = { param ~ ("," ~ param )* }
        pair.into_inner()
            .filter(|p: &Pair<Rule>| p.as_rule() == Rule::param)
            .map(|p: Pair<Rule>| {
                let mut inner = p.into_inner();
                // SAFETY: Grammar guarantees param has identifier and type
                let name = inner.next().unwrap().as_str().to_string();
                let type_node = inner.next().unwrap();
                let ty = self.parse_type(type_node)?;
                Ok(Param { name, ty })
            })
            .collect()
    }

    fn parse_block(&self, pair: Pair<Rule>) -> CompileResult<Block> {
        // block = { "{" ~ (statement)* ~ "}" }
        let stmts = pair
            .into_inner()
            // grammar gives us a wrapper Rule::statement
            .filter(|p: &Pair<Rule>| p.as_rule() == Rule::statement)
            .map(|stmt_pair: Pair<Rule>| {
                // SAFETY: Grammar guarantees exactly one child in statement wrapper
                let inner = stmt_pair.into_inner().next().unwrap();
                match inner.as_rule() {
                    Rule::var_decl => self.parse_var_decl(inner),
                    Rule::expr_stmt => self.parse_expr_stmt(inner),
                    Rule::return_stmt => self.parse_return(inner),
                    Rule::if_stmt => self.parse_if_stmt(inner),
                    Rule::while_stmt => self.parse_while_stmt(inner),
                    Rule::for_stmt => self.parse_for_stmt(inner),
                    Rule::break_stmt => Ok(Stmt::Break),
                    Rule::continue_stmt => Ok(Stmt::Continue),
                    other => Err(CompilationError::ParseError(format!(
                        "unexpected statement: {:?}",
                        other
                    ))),
                }
            })
            .collect::<Result<_, _>>()?; // Collect and propagate errors
        Ok(Block { stmts })
    }

    fn parse_var_decl(&self, pair: Pair<Rule>) -> CompileResult<Stmt> {
        // var_decl = { ("var"|"const") ~ ident ~ (":" ~ type_annotation)? ~ ("=" ~ expr)? ~ ";" }
        let mut name: Option<String> = None;
        let mut ty: Option<Type> = None;
        let mut init: Option<Expr> = None;

        for child in pair.into_inner() {
            match child.as_rule() {
                Rule::identifier if name.is_none() => {
                    // first identifier is the var name
                    name = Some(child.as_str().to_string());
                }
                Rule::type_annotation => {
                    // e.g. ": CString"
                    ty = Some(self.parse_type(child)?);
                }
                Rule::expr => {
                    init = Some(self.parse_expr(child)?);
                }
                _ => { /* skip punctuation and the 'var'/'const' keyword */ }
            }
        }

        let name = name.ok_or(parse_error!("var_decl missing identifier"))?;
        Ok(Stmt::VarDecl { name, ty, init })
    }

    fn parse_type(&self, pair: Pair<Rule>) -> CompileResult<Type> {
        let inner_rule = pair.into_inner().next().unwrap(); // Get the actual type rule (base_type, pointer_type, etc.)
        match inner_rule.as_rule() {
            Rule::base_type => {
                let mut inner_base_type = inner_rule.into_inner();
                let base_name = inner_base_type.next().unwrap().as_str().trim();
                // For now, assume no complex array types directly from parsing this,
                // and defer full array parsing if needed.
                Ok(Type::from_kit(base_name))
            }
            Rule::pointer_type => {
                let inner_ptr_type = inner_rule.into_inner().next().unwrap(); // Get the type being pointed to
                let inner_ty = self.parse_type(inner_ptr_type)?;
                Ok(Type::Ptr(Box::new(inner_ty)))
            }
            // TODO: Handle other type_annotation rules like function_type, tuple_type
            _ => Err(CompilationError::ParseError(format!(
                "Unexpected rule in type_annotation: {:?}",
                inner_rule.as_rule()
            ))),
        }
    }

    fn parse_expr_stmt(&self, pair: Pair<Rule>) -> CompileResult<Stmt> {
        // expr_stmt = { expr ~ ";" }
        // SAFETY: Grammar guarantees expression exists as first child
        let expr_pair = pair.into_inner().next().unwrap();
        Ok(Stmt::Expr(self.parse_expr(expr_pair)?))
    }

    fn parse_return(&self, pair: Pair<Rule>) -> CompileResult<Stmt> {
        // return_stmt = { "return" ~ expr? ~ ";" }
        let mut inner = pair.into_inner();
        let expr = inner.next().map(|p| self.parse_expr(p)).transpose()?;
        Ok(Stmt::Return(expr))
    }

    fn parse_if_stmt(&self, pair: Pair<Rule>) -> CompileResult<Stmt> {
        // if_stmt = { "if" ~ expr ~ block ~ else_part? }
        // else_part = { "else" ~ (block | if_stmt) }
        let mut inner = pair.into_inner();
        let cond = self.parse_expr(inner.next().unwrap())?;
        let then_branch = self.parse_block(inner.next().unwrap())?;

        let mut else_branch = None;
        if let Some(else_pair) = inner.next() {
            assert_eq!(else_pair.as_rule(), Rule::else_part);
            let else_content = else_pair.into_inner().next().unwrap();
            let else_block = match else_content.as_rule() {
                Rule::block => self.parse_block(else_content)?,
                Rule::if_stmt => {
                    let if_stmt = self.parse_if_stmt(else_content)?;
                    Block {
                        stmts: vec![if_stmt],
                    }
                }
                _ => unreachable!(),
            };
            else_branch = Some(else_block);
        }

        Ok(Stmt::If {
            cond,
            then_branch,
            else_branch,
        })
    }

    fn parse_while_stmt(&self, pair: Pair<Rule>) -> CompileResult<Stmt> {
        // while_stmt = { "while" ~ expr ~ block }
        let mut inner = pair.into_inner();
        let cond = self.parse_expr(inner.next().unwrap())?;
        let body = self.parse_block(inner.next().unwrap())?;
        Ok(Stmt::While { cond, body })
    }

    fn parse_for_stmt(&self, pair: Pair<Rule>) -> CompileResult<Stmt> {
        // for_stmt = { "for" ~ identifier ~ "in" ~ expr ~ block }
        let mut inner = pair.into_inner();
        let var = inner.next().unwrap().as_str().to_string();
        let iter = self.parse_expr(inner.next().unwrap())?;
        let body = self.parse_block(inner.next().unwrap())?;
        Ok(Stmt::For { var, iter, body })
    }

    fn parse_expr(&self, pair: Pair<Rule>) -> CompileResult<Expr> {
        match pair.as_rule() {
            Rule::expr => {
                // SAFETY: Grammar guarantees exactly one child for wrapper rules
                let inner = pair.into_inner().next().unwrap();
                self.parse_expr(inner)
            }
            Rule::assign => self.parse_assign_expr(pair),
            Rule::logical_or
            | Rule::logical_and
            | Rule::equality
            | Rule::comparison
            | Rule::additive
            | Rule::multiplicative
            | Rule::bitwise_or
            | Rule::bitwise_xor
            | Rule::bitwise_and
            | Rule::shift => {
                let mut inner = pair.into_inner();
                let mut left = self.parse_expr(inner.next().unwrap())?;

                while let Some(op_pair) = inner.next() {
                    let op = BinaryOperator::from_rule_pair(&op_pair)?;
                    let right = self.parse_expr(inner.next().unwrap())?;
                    left = Expr::BinaryOp {
                        op,
                        left: Box::new(left),
                        right: Box::new(right),
                    };
                }
                Ok(left)
            }
            Rule::unary => {
                let mut inner_pairs = pair.into_inner();
                // SAFETY: Unary rule always has at least one child
                let first_pair = inner_pairs.next().unwrap();

                match first_pair.as_rule() {
                    Rule::unary_op => {
                        let op_str = first_pair.as_str();
                        let op = UnaryOperator::from_str(op_str)
                            .map_err(|_| parse_error!("invalid unary operation: {}", op_str))?;

                        // SAFETY: Grammar guarantees expression after unary op
                        let expr = self.parse_expr(inner_pairs.next().unwrap())?;
                        Ok(Expr::UnaryOp {
                            op,
                            expr: Box::new(expr),
                        })
                    }
                    Rule::ADDRESS_OF_OP => {
                        // Handle address-of operator explicitly
                        let op = UnaryOperator::AddressOf;
                        let expr = self.parse_expr(inner_pairs.next().unwrap())?;
                        Ok(Expr::UnaryOp {
                            op,
                            expr: Box::new(expr),
                        })
                    }
                    Rule::primary => self.parse_expr(first_pair),
                    _other => Err(parse_error!("Unexpected rule in unary: {:?}", _other)),
                }
            }
            Rule::identifier => Ok(Expr::Identifier(pair.as_str().to_string())),
            Rule::literal => {
                // SAFETY: Grammar guarantees exactly one child in literal
                let inner = pair.into_inner().next().unwrap();
                match inner.as_rule() {
                    Rule::number => {
                        // SAFETY: Number always has exactly one child (integer/float)
                        let num_pair = inner.into_inner().next().unwrap();
                        match num_pair.as_rule() {
                            Rule::integer => {
                                let s = num_pair.as_str();
                                let i = s.parse::<i64>().map_err(|e| {
                                    parse_error!("invalid integer literal '{s}': {:?}", e)
                                })?;
                                Ok(Expr::Literal(Literal::Int(i)))
                            }
                            Rule::float => {
                                let s = num_pair.as_str();
                                let f = s.parse::<f64>().map_err(|e| {
                                    parse_error!("invalid float literal '{s}': {:?}", e)
                                })?;
                                Ok(Expr::Literal(Literal::Float(f)))
                            }
                            _ => Err(parse_error!("Unexpected number type")),
                        }
                    }
                    Rule::boolean => match inner.as_str() {
                        "true" => Ok(Expr::Literal(Literal::Bool(true))),
                        "false" => Ok(Expr::Literal(Literal::Bool(false))),
                        _s => Err(parse_error!("invalid boolean literal: {}", _s)),
                    },
                    Rule::char_literal => todo!("char literal parsing not implemented"),
                    _ => Err(parse_error!(
                        "Unexpected literal type: {:?}",
                        inner.as_rule()
                    )),
                }
            }
            Rule::string => {
                let full = pair.as_str();
                let inner = &full[1..full.len() - 1];
                let unescaped = Self::unescape(inner).unwrap_or_else(|| inner.to_string());
                Ok(Expr::Literal(Literal::String(unescaped)))
            }
            Rule::function_call_expr => {
                let mut inner = pair.into_inner();
                // SAFETY: Grammar guarantees callee identifier exists
                let callee = inner.next().unwrap().as_str().to_string();
                let args = inner
                    .filter(|p: &Pair<Rule>| p.as_rule() == Rule::expr)
                    .map(|p: Pair<Rule>| self.parse_expr(p))
                    .collect::<Result<Vec<_>, _>>()?; // Collect and propagate errors
                Ok(Expr::Call { callee, args })
            }
            Rule::if_expr => {
                let mut inner = pair.into_inner();
                let cond = self.parse_expr(inner.next().unwrap())?;
                let then_branch = self.parse_expr(inner.next().unwrap())?;
                let else_branch = self.parse_expr(inner.next().unwrap())?;
                Ok(Expr::If {
                    cond: Box::new(cond),
                    then_branch: Box::new(then_branch),
                    else_branch: Box::new(else_branch),
                })
            }

            Rule::primary => {
                // SAFETY: Primary rule always has exactly one child
                let inner = pair.into_inner().next().unwrap();
                self.parse_expr(inner)
            }
            Rule::range_expr => {
                let mut inner = pair.into_inner();
                let start = self.parse_expr(inner.next().unwrap())?;
                let end = self.parse_expr(inner.next().unwrap())?;
                Ok(Expr::RangeLiteral {
                    start: Box::new(start),
                    end: Box::new(end),
                })
            }
            other => Err(CompilationError::ParseError(format!(
                "Unexpected expr rule: {:?}",
                other
            ))),
        }
    }

    /// Parses an assignment expression (right-associative).
    fn parse_assign_expr(&self, pair: Pair<Rule>) -> CompileResult<Expr> {
        let mut inner = pair.into_inner();
        let left_pair = inner.next().unwrap(); // This is always the LHS (logical_or)

        let left = self.parse_expr(left_pair)?;

        // The grammar rule for `assign` is `assign = { logical_or ~ ASSIGN_OP ~ assign | logical_or }`.
        // This means `inner.next()` after the LHS (`logical_or`) will yield `ASSIGN_OP` if it's an assignment.
        // If there's no assignment, `inner` will be exhausted.

        if let Some(assign_op_pair) = inner.next() {
            // This should be ASSIGN_OP
            let op = AssignmentOperator::from_rule_pair(&assign_op_pair)?;
            let right_assign_expr_pair = inner.next().unwrap(); // This should be the RHS (nested assign)
            let right = self.parse_assign_expr(right_assign_expr_pair)?;
            Ok(Expr::Assign {
                op,
                left: Box::new(left),
                right: Box::new(right),
            })
        } else {
            // No assignment operator, so it's just the expression itself (the logical_or that formed the LHS)
            Ok(left)
        }
    }

    fn unescape(s: impl AsRef<str>) -> Option<String> {
        // TODO: search if there is an escape sequence beforehand to
        // avoid the allocation and search logic
        let s = s.as_ref();
        let mut out = String::with_capacity(s.len());
        let mut chars = s.chars();
        while let Some(c) = chars.next() {
            if c == '\\' {
                match chars.next()? {
                    'n' => out.push('\n'),
                    'r' => out.push('\r'),
                    't' => out.push('\t'),
                    '\\' => out.push('\\'),
                    '\'' => out.push('\''),
                    '"' => out.push('"'),
                    // For this grammar, `\` escapes any character
                    other => out.push(other),
                }
            } else {
                out.push(c);
            }
        }
        Some(out)
    }
}
