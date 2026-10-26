//! Lazy, repeatable generator values and their private execution cursors.
//! Continuations keep operand stacks and lexical scopes alive across `yield`.
use super::*;
use std::{
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
};

#[derive(Clone)]
pub struct Generator(Rc<Definition>);
struct Definition {
    body: Block,
    env: Environment,
}
impl Generator {
    pub(crate) fn new(body: Block, env: &Environment) -> Self {
        Self(Rc::new(Definition {
            body,
            env: env.snapshot(),
        }))
    }
    pub(crate) fn iter(&self) -> GeneratorIterator {
        GeneratorIterator {
            env: self.0.env.clone(),
            stack: Vec::new(),
            tasks: vec![Task::Eval(self.0.body.clone(), 0)],
            done: false,
        }
    }
}
impl fmt::Debug for Generator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Generator")
    }
}
impl fmt::Display for Generator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "generator")
    }
}
impl PartialEq for Generator {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}
impl Eq for Generator {}
impl PartialOrd for Generator {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Generator {
    fn cmp(&self, other: &Self) -> Ordering {
        Rc::as_ptr(&self.0).cmp(&Rc::as_ptr(&other.0))
    }
}
impl Hash for Generator {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0).hash(state);
    }
}
impl object::PrefixOperable for Generator {}
impl object::InfixOperable for Generator {
    fn equality(&self, other: &Object) -> Option<Object> {
        Some(matches!(other, Object::Generator(g) if self == g).into())
    }
    fn neq(&self, other: &Object) -> Option<Object> {
        Some((!matches!(other, Object::Generator(g) if self == g)).into())
    }
}

enum Task {
    Eval(Block, usize),
    PopScope,
    Case(Vec<(RuntimePattern, Block)>, Position),
    StartLoop(RuntimePattern, Block, Position),
    Loop(ObjectIterator, RuntimePattern, Block, Position, bool),
    StartComprehension(Rc<str>, Block, ComprehensionKind, Position),
    Comprehension(
        ObjectIterator,
        Rc<str>,
        Block,
        ComprehensionKind,
        Vec<Object>,
        bool,
    ),
    MethodReceiver(Block, Block, Block, Option<Rc<str>>, Position),
    Attribute(Rc<str>, Position),
    InvokeMethod(Option<Object>, Position, Position),
}

pub(crate) struct GeneratorIterator {
    env: Environment,
    stack: Vec<Object>,
    tasks: Vec<Task>,
    done: bool,
}
impl GeneratorIterator {
    fn iterable(&mut self, position: Position) -> ExecResult<ObjectIterator> {
        let value = pop(&mut self.stack);
        let kind = value.kind();
        ObjectIterator::new(value).ok_or_else(|| {
            Error::with_position(
                ExecError::NonIterableObject(kind).into(),
                position,
                self.env.file_path(),
            )
        })
    }

    fn resume(&mut self) -> ExecResult<Option<Object>> {
        loop {
            let Some(task) = self.tasks.pop() else {
                // A generator returned in tail position delegates its sequence.
                // Replace the cursor rather than nesting iterators: recursive
                // generators run with bounded continuation depth.
                if let Object::Generator(next) = pop(&mut self.stack) {
                    *self = next.iter();
                    continue;
                }
                return Ok(None);
            };
            match task {
                Task::Eval(block, offset) => {
                    let instruction = &block.instructions()[offset];
                    if offset + 1 < block.instructions().len() {
                        self.tasks.push(Task::Eval(block.clone(), offset + 1));
                    }
                    match &instruction.op {
                        Op::Prefix(PrefixOperator::Yield) => {
                            let value = pop(&mut self.stack);
                            self.stack.push(Object::empty_tuple());
                            return Ok(Some(value));
                        }
                        Op::If { positive, negative } => {
                            let branch = if truthy(&pop(&mut self.stack)) {
                                positive
                            } else {
                                negative
                            };
                            self.tasks.push(Task::Eval(branch.clone(), 0));
                        }
                        Op::Case { expr, pairs } => {
                            self.tasks.push(Task::Case(pairs.clone(), expr.position));
                            self.tasks.push(Task::Eval(expr.clone(), 0));
                        }
                        Op::For {
                            pattern,
                            iterator,
                            body,
                        } => {
                            self.tasks.push(Task::StartLoop(
                                pattern.clone(),
                                body.clone(),
                                iterator.position,
                            ));
                            self.tasks.push(Task::Eval(iterator.clone(), 0));
                        }
                        Op::Comprehension {
                            element,
                            variable,
                            iterator,
                            kind,
                        } => {
                            self.tasks.push(Task::StartComprehension(
                                variable.clone(),
                                element.clone(),
                                *kind,
                                iterator.position,
                            ));
                            self.tasks.push(Task::Eval(iterator.clone(), 0));
                        }
                        Op::MethodCall {
                            receiver,
                            method,
                            argument,
                            attribute,
                        } => {
                            self.tasks.push(Task::MethodReceiver(
                                receiver.clone(),
                                method.clone(),
                                argument.clone(),
                                attribute.clone(),
                                instruction.position,
                            ));
                            self.tasks.push(Task::Eval(receiver.clone(), 0));
                        }
                        _ => exec_instruction(instruction, &mut self.env, &mut self.stack)?,
                    }
                }
                Task::PopScope => self.env.pop_scope(),
                Task::Case(pairs, position) => {
                    let value = pop(&mut self.stack);
                    let mut matched = false;
                    for (pattern, body) in pairs {
                        if let Some(Match(bindings)) = pattern.matches(&value) {
                            self.env.push_bindings(ScopeKind::Block, bindings);
                            self.tasks.push(Task::PopScope);
                            self.tasks.push(Task::Eval(body, 0));
                            matched = true;
                            break;
                        }
                    }
                    if !matched {
                        return Err(Error::with_position(
                            ExecError::UnmatchedExpression.into(),
                            position,
                            self.env.file_path(),
                        ));
                    }
                }
                Task::StartLoop(pattern, body, position) => {
                    let iter = self.iterable(position)?;
                    self.env.push_scope(ScopeKind::Loop);
                    self.tasks
                        .push(Task::Loop(iter, pattern, body, position, false));
                }
                Task::Loop(mut iter, pattern, body, position, discard) => {
                    if discard {
                        pop(&mut self.stack);
                    }
                    if let Some(value) = iter.next().transpose()? {
                        for (name, value) in destructure(&pattern, position, &value, &mut self.env)?
                        {
                            self.env.set_inmutable(&name, value);
                        }
                        self.tasks
                            .push(Task::Loop(iter, pattern, body.clone(), position, true));
                        self.tasks.push(Task::Eval(body, 0));
                    } else {
                        self.env.pop_scope();
                        self.stack.push(Object::empty_tuple());
                    }
                }
                Task::StartComprehension(variable, body, kind, position) => {
                    let iter = self.iterable(position)?;
                    self.env.push_scope(ScopeKind::Block);
                    self.tasks.push(Task::Comprehension(
                        iter,
                        variable,
                        body,
                        kind,
                        Vec::new(),
                        false,
                    ));
                }
                Task::Comprehension(mut iter, variable, body, kind, mut values, collect) => {
                    if collect {
                        values.push(pop(&mut self.stack));
                    }
                    if let Some(value) = iter.next().transpose()? {
                        self.env.set_inmutable(&variable, value);
                        self.tasks.push(Task::Comprehension(
                            iter,
                            variable,
                            body.clone(),
                            kind,
                            values,
                            true,
                        ));
                        self.tasks.push(Task::Eval(body, 0));
                    } else {
                        self.env.pop_scope();
                        self.stack.push(match kind {
                            ComprehensionKind::List => Object::List(values.into()),
                            ComprehensionKind::Set => Object::Set(values.into()),
                        });
                    }
                }
                Task::MethodReceiver(receiver, method, argument, attribute, position) => {
                    let value = pop(&mut self.stack);
                    if matches!(value, Object::Dictionary(_)) {
                        self.tasks
                            .push(Task::InvokeMethod(None, receiver.position, position));
                        self.tasks.push(Task::Eval(argument, 0));
                        self.tasks.push(Task::Attribute(
                            attribute.expect("attribute must be a symbol"),
                            method.position,
                        ));
                        self.tasks.push(Task::Eval(receiver, 0));
                    } else {
                        self.tasks.push(Task::InvokeMethod(
                            Some(value),
                            receiver.position,
                            position,
                        ));
                        self.tasks.push(Task::Eval(argument, 0));
                        self.tasks.push(Task::Eval(method, 0));
                    }
                }
                Task::Attribute(name, position) => {
                    let value = pop(&mut self.stack);
                    self.stack
                        .push(object_attr(value, &name, position, &self.env)?);
                }
                Task::InvokeMethod(receiver, callee, position) => {
                    let mut argument = pop(&mut self.stack);
                    let function = pop(&mut self.stack);
                    if let Some(receiver) = receiver {
                        let mut values = match argument {
                            Object::Tuple(tuple) => tuple.list.into_owned(),
                            obj => vec![obj],
                        };
                        values.insert(0, receiver);
                        argument = if values.len() == 1 {
                            values.pop().unwrap()
                        } else {
                            Object::Tuple(values.into())
                        };
                    }
                    self.stack.push(call(
                        function,
                        argument,
                        None,
                        callee,
                        position,
                        &mut self.env,
                    )?);
                }
            }
        }
    }
}
impl Iterator for GeneratorIterator {
    type Item = ExecResult<Object>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }
        match self.resume() {
            Ok(Some(value)) => Some(Ok(value)),
            result => {
                self.done = true;
                self.tasks.clear();
                self.stack.clear();
                self.env = Environment::default();
                result.err().map(Err)
            }
        }
    }
}
