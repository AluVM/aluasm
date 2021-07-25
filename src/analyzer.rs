// AluVM Assembler
// To find more on AluVM please check <https://www.aluvm.org>
//
// Designed & written in 2021 by
//     Dr. Maxim Orlovsky <orlovsky@pandoracore.com>
// for Pandora Core AG

//! Analyzer code coverting abstract parse tree, returned by parser, into an
//! abstract syntax tree (AST)

use std::collections::{BTreeMap, BTreeSet};
use std::str::FromStr;

use aluvm::libs::LibId;
use aluvm::Isa;
use pest::iterators::{Pair, Pairs};
use pest::Span;

use crate::ast::{Const, FlagSet, Operator, Routine, Var};
use crate::{Mnemonic, Rule};

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, Display)]
#[display(doc_comments)]
pub enum Error {
    /// re-definition of `{0}` routine
    RepeatedRoutineName(String),

    /// unknown operation: `{0}`
    UnknownMnemonic(String),

    /// repeated label `{label}` inside `{routine}` routine
    RepeatedLabel { label: String, routine: String },
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug, Display)]
#[display(doc_comments)]
pub enum Warning {}

#[derive(Clone, Hash, Default, Debug)]
pub struct Issues<'i> {
    pub errors: Vec<(Error, Span<'i>)>,
    pub warnings: Vec<(Warning, Span<'i>)>,
}

impl<'i> Issues<'i> {
    pub fn push_error(&mut self, error: Error, span: Span<'i>) {
        self.errors.push((error, span));
    }
}

#[derive(Clone, Hash, Default, Debug)]
pub struct Program<'i> {
    pub isae: BTreeSet<Isa>,
    pub libs: BTreeMap<String, LibId>,
    pub main: Option<Routine>,
    pub code: BTreeMap<String, Routine>,
    pub r#const: BTreeMap<String, Const>,
    pub input: BTreeMap<String, Var>,
    pub issues: Issues<'i>,
}

impl<'i> Program<'i> {
    pub fn analyze(pair: Pair<'i, Rule>) -> Self {
        let mut program = Program::default();
        for pair in pair.into_inner() {
            match pair.as_rule() {
                Rule::isae => program.analyze_isae(pair),
                Rule::routine => program.analyze_routine(pair),
                Rule::libs => program.analyze_libs(pair),
                Rule::data => program.analyze_const(pair),
                Rule::input => program.analyze_input(pair),
                Rule::EOI => {}
                _ => unreachable!(
                    "lexer fatal error: unknown program segment `{:?}`",
                    pair.as_rule()
                ),
            };
        }
        program
    }
}

impl<'i> Program<'i> {
    fn analyze_isae(&mut self, pair: Pair<'i, Rule>) {
        for pair in pair.into_inner() {}
    }

    fn analyze_routine(&mut self, pair: Pair<'i, Rule>) {
        let span = pair.as_span();
        let routine = Routine::analyze(pair, &mut self.issues);
        if self.code.contains_key(&routine.name) {
            self.issues
                .push_error(Error::RepeatedRoutineName(routine.name), span);
        } else {
            self.code.insert(routine.name.clone(), routine);
        }
    }

    fn analyze_libs(&mut self, pair: Pair<'i, Rule>) {
        for pair in pair.into_inner() {}
    }

    fn analyze_const(&mut self, pair: Pair<'i, Rule>) {
        for pair in pair.into_inner() {}
    }

    fn analyze_input(&mut self, pair: Pair<'i, Rule>) {
        for pair in pair.into_inner() {}
    }
}

trait Analyze {
    fn analyze<'i>(pair: Pair<'i, Rule>, issues: &mut Issues<'i>) -> Self;
}

impl Analyze for Routine {
    fn analyze<'i>(pair: Pair<'i, Rule>, issues: &mut Issues<'i>) -> Self {
        let mut iter = pair.into_inner();

        let routine_name = iter
            .next()
            .expect("lexer fatal error: routine must always has a name");
        let name = match routine_name.as_rule() {
            Rule::routine_main => ".MAIN",
            Rule::routine_name => routine_name.as_str(),
            _ => unreachable!(
                "lexer fatal error: routine must always start with a name"
            ),
        }
        .to_owned();

        let mut labels = bmap![];
        let code: Vec<Operator> = iter
            .enumerate()
            .map(|(index, pair)| {
                let span = pair.as_span();
                let op = Operator::analyze(pair, issues);
                if let Some(label) = op.label.clone() {
                    if labels.contains_key(&label) {
                        issues.push_error(
                            Error::RepeatedLabel {
                                label,
                                routine: name.clone(),
                            },
                            span,
                        );
                    } else {
                        labels.insert(label, index as u16);
                    }
                }
                op
            })
            .collect();

        Routine { name, labels, code }
    }
}

impl Analyze for Operator {
    fn analyze<'i>(pair: Pair<'i, Rule>, issues: &mut Issues<'i>) -> Self {
        let mut iter = pair.into_inner();
        let label = match iter.peek() {
            Some(pair) if pair.as_rule() == Rule::label => {
                let _ = iter.next();
                Some(pair.as_str().to_owned())
            }
            _ => None,
        };
        let pair = iter
            .next()
            .expect("lexer fatal error: label not followed by an instruction");
        let mnemonic = Mnemonic::from_str(pair.as_str()).unwrap_or_else(|_| {
            issues.push_error(
                Error::UnknownMnemonic(pair.as_str().to_owned()),
                pair.as_span(),
            );
            Mnemonic::nop
        });

        // TODO: Add flag and operand processing

        Operator {
            label,
            mnemonic,
            postfix_flags: FlagSet::None,
            keyed_flags: FlagSet::None,
            operands: vec![],
        }
    }
}
