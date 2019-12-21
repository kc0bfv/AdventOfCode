#[derive(Debug,Hash,PartialEq,Eq,Clone,PartialOrd,Ord)]
pub struct Position {
    pub x: usize,
    pub y: usize,
}
impl Position {
    pub fn new(x: usize, y: usize) -> Position {
        Position{ x, y }
    }
    /*
    fn manh_dist(&self, other: &Position) -> u64 {
        ((other.x - self.x).abs() + (other.y - self.y).abs()).try_into().unwrap()
    }
    fn diff(&self, other: &Position) -> (i64, i64) {
        (other.x - self.x, other.y - self.y)
    }
    fn add(&self, other: &Position) -> Position {
        Position::new(self.x + other.x, self.y + other.y)
    }
    fn in_vec<'a, T>(&self, vec: &'a Vec<Vec<T>>) -> &'a T {
        match vec.get(self.y as usize) {
            Some(vec_y) =>
                match vec_y.get(self.x as usize) {
                        Some(ref_val) => ref_val,
                        None => vec.get(0).unwrap().get(0).unwrap(),
                    },
            None => vec.get(0).unwrap().get(0).unwrap(),
        }
    }
    */
    pub fn get_surroundings(&self) -> Vec<Self> {
        vec![
                Position::new(self.x - 1, self.y + 0),
                Position::new(self.x + 1, self.y + 0),
                Position::new(self.x + 0, self.y - 1),
                Position::new(self.x + 0, self.y + 1),
            ]
    }
}
