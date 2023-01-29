use std::collections::HashSet;

#[derive(Debug)]
pub enum SearchError {
    FrontierEmptied,
    NoResult,
    CustomError(Box<dyn std::error::Error>),
}
impl std::fmt::Display for SearchError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SearchError::FrontierEmptied => write!(f, "Frontier emptied!"),
            SearchError::NoResult => write!(f, "No result!"),
            SearchError::CustomError(err) => write!(f, "Error: {}", err),
        }
    }
}
impl std::error::Error for SearchError {}

pub trait BreadthSearch<I> {
    fn is_complete<'a, B: 'a>(&self, collec: B) -> bool
        where B: IntoIterator<Item=&'a I>, I: 'a;
    fn gen_children(&self, item: I) -> Box<dyn Iterator<Item=I>>;
    fn find_result(&self, iter: &mut impl Iterator<Item=I>) -> Result<I, Box<dyn std::error::Error>>;
    
    fn breadth(&self, init_frontier: impl IntoIterator<Item=I>)
        -> Result<I, SearchError>
        where I: Eq + std::hash::Hash,
    {
        let mut frontier: HashSet<I> = HashSet::from_iter(init_frontier.into_iter());

        while !self.is_complete(frontier.iter()) && !frontier.is_empty() {
            frontier = HashSet::from_iter(frontier.into_iter().map(|val| self.gen_children(val) ).flatten());
        }

        if frontier.is_empty() {
            return Err(SearchError::FrontierEmptied);
        }
        self.find_result(&mut frontier.into_iter()).map_err(|err| SearchError::CustomError(err))
    }
}

#[cfg(test)]
mod search_test {
    use crate::search;

    #[test]
    fn fib_search() {
        #[derive(Hash, PartialEq, Eq)]
        struct FibStruct {
            prev0: u32,
            prev1: u32,
        }
        #[derive(Debug, Copy, Clone)]
        pub enum FibStructError {
            TwoResults,
        }
        impl std::fmt::Display for FibStructError {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    FibStructError::TwoResults => write!(f, "Two Results"),
                }
            }
        }
        impl std::error::Error for FibStructError {}

        struct FibSearch {}
        use search::BreadthSearch;
        impl BreadthSearch<FibStruct> for FibSearch {
            fn is_complete<'a, B>(&self, collec: B) -> bool
                where B: IntoIterator<Item=&'a FibStruct>
            {
                collec.into_iter().any(|val| val.prev1 > 100)
            }
            fn gen_children(&self, item: FibStruct) -> Box<dyn Iterator<Item=FibStruct>> {
                let next_f = FibStruct {
                    prev0: item.prev1,
                    prev1: item.prev0 + item.prev1
                };
                Box::new(vec![next_f].into_iter())
            }
            fn find_result(&self, iter: &mut impl Iterator<Item=FibStruct>)
                -> Result<FibStruct, Box<dyn std::error::Error>>
            {
                let mut filtered = iter.filter(|val| val.prev1 > 100);
                let first = filtered.next();
                let second = filtered.next();

                if !second.is_none() { return Err(Box::new(FibStructError::TwoResults)); }

                match first {
                    Some(val) => Ok(val),
                    None => Err(Box::new(search::SearchError::NoResult)),
                }
            }
        }

        let init_frontier = vec![ FibStruct{ prev0: 0, prev1: 1 } ];

        let result = FibSearch {}.breadth(init_frontier);
        assert_eq!(result.as_ref().unwrap().prev1, 144);

        match result {
            Ok(fib) => println!("Result: {}", fib.prev1),
            Err(err) => println!("Err: {}", err),
        };

    }
}
