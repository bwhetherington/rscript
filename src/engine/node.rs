use crate::parser::{Module, Statement};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

type Ptr<T> = Rc<RefCell<T>>;

type Str = Rc<str>;

pub fn ptr<T>(el: T) -> Ptr<T> {
    Rc::new(RefCell::new(el))
}

#[derive(Debug, Clone)]
pub enum Node {
    Module(Str, Ptr<HashMap<Str, Node>>),
    Item(Str),
}

impl Node {
    pub fn module(name: impl Into<Str>) -> Node {
        let name = name.into();
        let children = HashMap::new();
        Node::Module(name, ptr(children))
    }

    pub fn trace(module: &Module) -> Node {
        let name = module.identifier.to_string().into();

        // Create child nodes
        let mut children = HashMap::new();

        // Create child items
        for statement in &module.body {
            match statement {
                Statement::Assignment {
                    visibility,
                    parameter,
                    ..
                } if visibility.is_visible() => {
                    // Create item
                    let child_name: Str = parameter.to_string().into();
                    let child_node = Node::Item(child_name.clone());
                    children.insert(child_name, child_node);
                }
                Statement::FunctionDeclaration {
                    visibility,
                    identifier,
                    ..
                } if visibility.is_visible() => {
                    // Create item
                    let child_name: Str = identifier.to_string().into();
                    let child_node = Node::Item(child_name.clone());
                    children.insert(child_name, child_node);
                }
                _ => (),
            }
        }

        // Create child modules
        for child_module in &module.children {
            let child_node = Node::trace(child_module);
            let child_name: Str = child_node.name().into();
            children.insert(child_name, child_node);
        }

        Node::Module(name, ptr(children))
    }

    pub fn name(&self) -> Str {
        match self {
            Node::Module(name, _) => name.clone(),
            Node::Item(name) => name.clone(),
        }
    }

    /// Traces through the module to find the node with the specified relative
    /// path.
    pub fn find(&self, path: &[String]) -> Option<Node> {
        if let Some((first, rest)) = path.split_first() {
            match self {
                Node::Module(_, children) => {
                    // Foo
                    let children = children.borrow();
                    children
                        .get(first.as_str())
                        .and_then(|child| child.find(rest))
                }
                Node::Item(_) => None,
            }
        } else {
            // We have reached the end
            Some(self.clone())
        }
    }

    pub fn find_items(&self, prefix: &[String]) -> Vec<Vec<String>> {
        let mut paths = Vec::new();

        let module = self.find(prefix);
        if let Some(Node::Module(_, children)) = module {
            for (_, child) in children.borrow().iter() {
                if let Node::Item(name) = child {
                    let mut path = prefix.to_vec();
                    path.push(name.as_ref().to_owned());
                    paths.push(path);
                }
            }
        }

        paths
    }

    pub fn contains(&self, path: &[String]) -> bool {
        self.find(path).is_some()
    }

    pub fn insert_child(&mut self, child: Node) -> bool {
        match self {
            Node::Module(_, children) => {
                let mut children = children.borrow_mut();
                let name = child.name();
                children.insert(name, child);
                true
            }
            _ => false,
        }
    }
}
