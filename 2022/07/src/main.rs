use std::cell::RefCell;
use std::rc::{Rc, Weak};

#[derive(Copy, Clone, Debug)]
enum FType {
    Dir(Option<u32>),
    File(u32)
}

#[derive(Debug)]
struct Node {
    name: String,
    ftype: FType,
    parent: Option<Weak<RefCell<Node>>>,
    children: Vec<Rc<RefCell<Node>>>,
}

impl Node {
    fn new(name: String, ftype: FType, parent: Option<Weak<RefCell<Node>>>) -> Self {
        Self {
            name: name,
            ftype: ftype,
            parent: parent,
            children: Vec::new()
        }
    }
}

trait AddNodeIter {
    fn iter(&self) -> NodeIter;
    fn get_child(&self, name: String) -> Option<Rc<RefCell<Node>>>;
    fn get_size(&self) -> u32;
}

impl AddNodeIter for Rc<RefCell<Node>> {
    fn iter(&self) -> NodeIter {
        NodeIter::new(self.clone())
    }
    fn get_child(&self, name: String) -> Option<Rc<RefCell<Node>>> {
        for child in self.borrow().children.iter() {
            if child.borrow().name == name {
                return Some(child.clone());
            }
        }

        None
    }
    fn get_size(&self) -> u32 {
        let ftype = self.borrow().ftype;
        match ftype {
            FType::File(size) => size,
            FType::Dir(Some(size)) => size,
            FType::Dir(None) => {
                    let size = self.borrow().children.iter().map(|c| c.get_size()).sum();
                    self.borrow_mut().ftype = FType::Dir(Some(size));
                    return size;
                }
        }
    }
}

struct NodeIter {
    root: Rc<RefCell<Node>>,
    cur: Option<Rc<RefCell<Node>>>,
}

impl NodeIter {
    fn new(root: Rc<RefCell<Node>>) -> Self {
        Self { root: root, cur: None }
    }
}

impl Iterator for NodeIter {
    type Item = Rc<RefCell<Node>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cur.is_none() {
            self.cur = Some(self.root.clone());
            return Some(self.root.clone());
        }

        let cur = self.cur.as_ref().unwrap().clone();

        // go into any children first
        if cur.borrow().children.len() > 0 {
            self.cur = Some(cur.borrow().children[0].clone());
            return Some(cur.borrow().children[0].clone());
        }


        // Go up to the parent and try the next there.
        let mut up_cur = cur;
        loop {
            // If no parent, all done
            if up_cur.borrow().parent.is_none() {
                return None
            }
            let parent = up_cur.borrow()
                .parent.as_ref().unwrap()
                .clone().upgrade().expect("Upgrade failed in up_cur");
                
            // Search through the parent's children until we find the child we were last at
            // Then go to the child after that
            let mut is_next = false;
            for child in &parent.borrow().children {
                if is_next {
                    self.cur = Some(child.clone());
                    return Some(child.clone());
                }
                is_next = Rc::ptr_eq(&child, &up_cur);
            }

            // The child we were last at must've been the last child on the parent, go up
            up_cur = parent.clone();
        }
    }
}

fn runit(filename: &str) -> Result<(), Box<dyn std::error::Error>> {
    let raw_content = std::fs::read_to_string(filename)?;
    let content: Vec<&str> = raw_content.lines()
        .map(|s| { s.trim().into() })
        .collect();

    let root = Rc::new(RefCell::new(Node::new("".into(), FType::Dir(None), None)));
    let mut current = root.clone();

    // Handle all inputs
    for cmd in content {
        let splitcmd: Vec<_> = cmd.split(" ").collect();

        // If it's not a command...
        if splitcmd[0] != "$" {
            let child = match splitcmd[0] {
                "dir" => Node::new(splitcmd[1].into(),
                    FType::Dir(None), Some(Rc::downgrade(&current))),
                _ => Node::new(splitcmd[1].into(),
                    FType::File(splitcmd[0].parse().expect("Parsing size failed")),
                    Some(Rc::downgrade(&current)))
            };
            current.borrow_mut().children.push(Rc::new(RefCell::new(child)));

            continue;
        }

        if splitcmd[1] == "ls" {
            continue;
        }

        // If it's a command and it's not ls, it's cd
        assert!(splitcmd[1] == "cd");

        if splitcmd[2] == "/" {
            continue;
        } else if splitcmd[2] == ".." {
            let parent_wk = current.borrow()
                .parent.as_ref().expect("Tried parent w/no parent").clone();
            current = parent_wk.upgrade().expect("Upgrade failed");
        } else {
            let child = current.get_child(splitcmd[2].into()).expect("Failed to find child");
            current = child;
        }
    }

    let size: u32 = root.iter().filter(|rc_ref_node| {
            let ftype = rc_ref_node.borrow().ftype;
            if let FType::Dir(_) = ftype {
                rc_ref_node.get_size() <= 100000
            } else {
                false
            }
        }).map(|rc_ref_node| rc_ref_node.get_size()).sum();
    println!("Part 1: {}", size);

    let fs_space   = 70000000;
    let need_space = 30000000;
    let max_used = fs_space - need_space;
    let cur_used = root.get_size();
    let to_free = cur_used.abs_diff(max_used);

    let min = NodeIter::new(root.clone()).filter(|rc_ref_node| {
            let b_typ = rc_ref_node.borrow().ftype;
            if let FType::Dir(_) = b_typ {
                rc_ref_node.get_size() >= to_free
            } else {
                false
            }
        }).min_by(|a, b| a.get_size().cmp(&b.get_size()))
        .expect("Failed to find min");
    println!("Part 2: {}", min.get_size());

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    runit("samp")?;
    runit("input.txt")?;

    Ok(())
}
