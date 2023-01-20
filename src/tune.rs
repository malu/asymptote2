use crate::{
    eval::{
        eg, mg, EScore, Eval, Score, BISHOP_MOBILITY, BISHOP_PST, DOUBLED_PAWN, KING_PST,
        KNIGHT_MOBILITY, KNIGHT_PST, PASSED_PAWN_ON_RANK, PAWN_PST, PAWN_SHIELD_KING_FILE,
        PAWN_SHIELD_OTHER_FILE, PAWN_STORM, PIECE_VALUES, QUEEN_PST, ROOK_MOBILITY, ROOK_PST, S,
        WEAK_PAWN,
    },
    position::Position,
    random::Xoshiro,
    types::{File, Piece, PieceMap, Rank, Side, Square, SquareMap},
};

#[derive(Clone, Debug)]
pub struct Trace {
    pub eval: EScore,
    pub sf: f32,
    result: f32,
    pub phase: i16,

    pub material: Material<false>,
    pub pawn_pst: SymmetricSquareMap<false>,
    pub knight_pst: SymmetricSquareMap<false>,
    pub bishop_pst: SymmetricSquareMap<false>,
    pub rook_pst: SymmetricSquareMap<false>,
    pub queen_pst: SymmetricSquareMap<false>,
    pub king_pst: SymmetricSquareMap<false>,

    pub knight_mobility: Array<false, 9>,
    pub bishop_mobility: Array<false, 14>,
    pub rook_mobility: Array<false, 15>,

    pub doubled_pawn: Single<false>,
    pub weak_pawn: Single<true>,
    pub passed_pawn_on_rank: Array<false, 8>,

    pub pawn_shield_king_file: Array<false, 8>,
    pub pawn_shield_other_file: Array<false, 8>,
    pub pawn_storm: Array<false, 8>,
}

impl Default for Trace {
    fn default() -> Self {
        Self {
            eval: EScore::default(),
            sf: 1.,
            result: 0.,
            phase: 0,

            material: Material::default(),
            pawn_pst: SymmetricSquareMap::new("PAWN_PST", &PAWN_PST),
            knight_pst: SymmetricSquareMap::new("KNIGHT_PST", &KNIGHT_PST),
            bishop_pst: SymmetricSquareMap::new("BISHOP_PST", &BISHOP_PST),
            rook_pst: SymmetricSquareMap::new("ROOK_PST", &ROOK_PST),
            queen_pst: SymmetricSquareMap::new("QUEEN_PST", &QUEEN_PST),
            king_pst: SymmetricSquareMap::new("KING_PST", &KING_PST),

            knight_mobility: Array::new("KNIGHT_MOBILITY", &KNIGHT_MOBILITY),
            bishop_mobility: Array::new("BISHOP_MOBILITY", &BISHOP_MOBILITY),
            rook_mobility: Array::new("ROOK_MOBILITY", &ROOK_MOBILITY),

            doubled_pawn: Single::new("DOUBLED_PAWN", &DOUBLED_PAWN),
            weak_pawn: Single::new("WEAK_PAWN", &WEAK_PAWN),
            passed_pawn_on_rank: Array::new("PASSED_PAWN_ON_RANK", &PASSED_PAWN_ON_RANK),

            pawn_shield_king_file: Array::new("PAWN_SHIELD_KING_FILE", &PAWN_SHIELD_KING_FILE),
            pawn_shield_other_file: Array::new("PAWN_SHIELD_OTHER_FILE", &PAWN_SHIELD_OTHER_FILE),
            pawn_storm: Array::new("PAWN_STORM", &PAWN_STORM),
        }
    }
}

impl Trace {
    fn new(position: &Position, result: f32) -> Self {
        // TODO run qsearch first
        let mut eval = Eval::from(position);
        // pawn hash doesn't matter and is disabled during tuning anyway
        let _ = eval.evaluate(position, 0);
        eval.trace.result = result;

        eval.trace
    }

    fn for_each_tunable<'a, F>(&'a self, mut f: F)
    where
        F: FnMut(Box<&'a dyn Tunable>),
    {
        f(Box::new(&self.material));
        f(Box::new(&self.pawn_pst));
        f(Box::new(&self.knight_pst));
        f(Box::new(&self.bishop_pst));
        f(Box::new(&self.rook_pst));
        f(Box::new(&self.queen_pst));
        f(Box::new(&self.king_pst));
        f(Box::new(&self.knight_mobility));
        f(Box::new(&self.bishop_mobility));
        f(Box::new(&self.rook_mobility));
        f(Box::new(&self.doubled_pawn));
        f(Box::new(&self.weak_pawn));
        f(Box::new(&self.passed_pawn_on_rank));
        f(Box::new(&self.pawn_shield_king_file));
        f(Box::new(&self.pawn_shield_other_file));
        f(Box::new(&self.pawn_storm));
    }
}

trait Tunable {
    fn enabled(&self) -> bool;
    fn compact(&self) -> Vec<i8>;
    fn schema(&self) -> Box<dyn Schema>;
}

trait Schema {
    fn num_parameters(&self) -> usize;
    fn initial_parameter_values(&self) -> Vec<EScore>;
    fn gradients(
        &self,
        parameters: &[(f32, f32)],
        trace: &[i8],
        f: f32,
        phase: f32,
        sf: f32,
        g: &mut [(f32, f32)],
    );
    fn constraints(&self, _parameters: &mut [(f32, f32)]);
    fn print(&self, values: &[(f32, f32)]);
}

struct Schemas {
    schemas: Vec<(Box<dyn Schema>, usize)>,
}

impl From<Trace> for Schemas {
    fn from(trace: Trace) -> Self {
        let mut schemas = vec![];
        let mut running_offset = 0;

        trace.for_each_tunable(|tunable| {
            if tunable.enabled() {
                schemas.push((tunable.schema(), running_offset));
                running_offset += tunable.schema().num_parameters();
            }
        });

        Self { schemas }
    }
}

struct CompactTrace {
    data: Vec<i8>,
    phase: f32,
    result: f32,
    sf: f32,
    base_eval: (f32, f32),
}

impl CompactTrace {
    fn new(trace: &Trace, parameters: &Parameters) -> Self {
        let mut data = vec![];

        trace.for_each_tunable(|tunable| {
            if tunable.enabled() {
                data.extend(tunable.compact());
            }
        });

        let mut t = Self {
            data,
            result: trace.result,
            sf: trace.sf,
            phase: trace.phase as f32,
            base_eval: (0., 0.),
        };

        // Eval score just considering the variables to be tuned
        let tuned_eval = parameters.eval_phases(&t);

        t.base_eval.0 = mg(trace.eval) as f32 - tuned_eval.0;
        t.base_eval.1 = eg(trace.eval) as f32 - tuned_eval.1;

        t
    }
}

struct Parameters {
    parameters: Vec<(f32, f32)>,
}

impl Parameters {
    fn eval(&self, trace: &CompactTrace) -> f32 {
        let phase = trace.phase;
        let score = self.eval_phases(trace);

        (score.0 * phase + score.1 * (62. - phase)) / 62.
    }

    fn eval_phases(&self, trace: &CompactTrace) -> (f32, f32) {
        let mut score = trace.base_eval;

        for (p, t) in self.parameters.iter().zip(&trace.data) {
            score.0 += p.0 * *t as f32;
            score.1 += p.1 * *t as f32;
        }

        score
    }
}

impl From<&Schemas> for Parameters {
    fn from(schemas: &Schemas) -> Self {
        let parameters = schemas
            .schemas
            .iter()
            .flat_map(|(schema, _)| {
                let initial_values = schema.initial_parameter_values();
                assert_eq!(initial_values.len(), schema.num_parameters());
                initial_values
                    .into_iter()
                    .map(|val| (mg(val) as f32, eg(val) as f32))
            })
            .collect();

        Self { parameters }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Single<const ENABLED: bool> {
    pub inner: [i8; 2],
    name: &'static str,
    source: &'static EScore,
}

impl<const ENABLED: bool> Single<ENABLED> {
    fn new(name: &'static str, source: &'static EScore) -> Self {
        Self {
            inner: Default::default(),
            name,
            source,
        }
    }
}

impl<const ENABLED: bool> Tunable for Single<ENABLED> {
    fn enabled(&self) -> bool {
        ENABLED
    }

    fn compact(&self) -> Vec<i8> {
        let w = Side::White as usize;
        let b = Side::Black as usize;
        vec![self.inner[w] - self.inner[b]]
    }

    fn schema(&self) -> Box<dyn Schema> {
        Box::new(SingleSchema {
            name: self.name,
            source: self.source,
        })
    }
}

struct SingleSchema {
    name: &'static str,
    source: &'static EScore,
}

impl Schema for SingleSchema {
    fn num_parameters(&self) -> usize {
        1
    }

    fn initial_parameter_values(&self) -> Vec<EScore> {
        vec![*self.source]
    }

    fn gradients(
        &self,
        _parameters: &[(f32, f32)],
        trace: &[i8],
        f: f32,
        phase: f32,
        sf: f32,
        g: &mut [(f32, f32)],
    ) {
        let t = trace[0] as f32;
        g[0].0 += t * f * phase / 62.;
        g[0].1 += sf * t * f * (62. - phase) / 62.;
    }

    fn constraints(&self, _parameters: &mut [(f32, f32)]) {}

    fn print(&self, values: &[(f32, f32)]) {
        println!("#[rustfmt::skip]");
        println!(
            "pub const {}: EScore = S({:>4}, {:>4});",
            self.name, values[0].0 as Score, values[0].1 as Score
        );
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Material<const ENABLED: bool>(pub [PieceMap<i8>; 2]);

impl<const ENABLED: bool> Tunable for Material<ENABLED> {
    fn enabled(&self) -> bool {
        ENABLED
    }

    fn compact(&self) -> Vec<i8> {
        let w = Side::White as usize;
        let b = Side::Black as usize;
        vec![
            self.0[w][Piece::Pawn] - self.0[b][Piece::Pawn],
            self.0[w][Piece::Knight] - self.0[b][Piece::Knight],
            self.0[w][Piece::Bishop] - self.0[b][Piece::Bishop],
            self.0[w][Piece::Rook] - self.0[b][Piece::Rook],
            self.0[w][Piece::Queen] - self.0[b][Piece::Queen],
        ]
    }

    fn schema(&self) -> Box<dyn Schema> {
        Box::new(MaterialSchema)
    }
}

struct MaterialSchema;

impl Schema for MaterialSchema {
    fn num_parameters(&self) -> usize {
        5
    }

    fn initial_parameter_values(&self) -> Vec<EScore> {
        vec![
            PIECE_VALUES[Piece::Pawn],
            PIECE_VALUES[Piece::Knight],
            PIECE_VALUES[Piece::Bishop],
            PIECE_VALUES[Piece::Rook],
            PIECE_VALUES[Piece::Queen],
        ]
    }

    fn gradients(
        &self,
        _parameters: &[(f32, f32)],
        trace: &[i8],
        f: f32,
        phase: f32,
        sf: f32,
        g: &mut [(f32, f32)],
    ) {
        // Only update pawn endgame value
        g[0].1 += sf * trace[0] as f32 * f * (62. - phase) / 62.;

        // Skip pawns update
        g.iter_mut()
            .skip(1)
            .zip(trace.iter().skip(1))
            .for_each(|(g, t)| {
                let t = *t as f32;
                g.0 += t * f * phase / 62.;
                g.1 += sf * t * f * (62. - phase) / 62.;
            });
    }

    fn constraints(&self, _parameters: &mut [(f32, f32)]) {}

    fn print(&self, values: &[(f32, f32)]) {
        println!("#[rustfmt::skip]");
        println!("pub const PIECE_VALUES: PieceMap<EScore> = PieceMap::new([");
        for i in 0..5 {
            println!(
                "    S({:>4}, {:>4}),",
                values[i].0 as Score, values[i].1 as Score
            );
        }
        println!("    S(   0,    0),");
        println!("]);");
    }
}

#[derive(Clone, Debug)]
pub struct SymmetricSquareMap<const ENABLED: bool> {
    pub inner: [SquareMap<i8>; 2],
    name: &'static str,
    source: &'static SquareMap<EScore>,
}

impl<const ENABLED: bool> SymmetricSquareMap<ENABLED> {
    fn new(name: &'static str, source: &'static SquareMap<EScore>) -> Self {
        Self {
            inner: Default::default(),
            name,
            source,
        }
    }
}

pub struct SymmetricSquareMapSchema {
    name: &'static str,
    source: &'static SquareMap<EScore>,
}

impl<const ENABLED: bool> Tunable for SymmetricSquareMap<ENABLED> {
    fn enabled(&self) -> bool {
        ENABLED
    }

    fn compact(&self) -> Vec<i8> {
        let w = Side::White as usize;
        let b = Side::Black as usize;
        let mut result = Vec::with_capacity(32);
        for rank in Rank::all() {
            for file in 0..4 {
                let west_file = File::new(file).unwrap();
                let east_file = File::new(7 - file).unwrap();

                let west_sq = Square::from_file_rank(west_file, rank);
                let east_sq = Square::from_file_rank(east_file, rank);

                result.push(
                    self.inner[w][west_sq] + self.inner[w][east_sq]
                        - self.inner[b][west_sq]
                        - self.inner[b][east_sq],
                );
            }
        }

        result
    }

    fn schema(&self) -> Box<dyn Schema> {
        Box::new(SymmetricSquareMapSchema {
            name: self.name,
            source: self.source,
        })
    }
}

impl Schema for SymmetricSquareMapSchema {
    fn num_parameters(&self) -> usize {
        32
    }

    fn initial_parameter_values(&self) -> Vec<EScore> {
        let mut result = Vec::with_capacity(32);
        for rank in Rank::all() {
            for file in 0..4 {
                let west_file = File::new(file).unwrap();
                let east_file = File::new(7 - file).unwrap();

                let west_sq = Square::from_file_rank(west_file, rank);
                let east_sq = Square::from_file_rank(east_file, rank);

                let west = self.source[west_sq];
                let east = self.source[east_sq];

                let v = S((mg(west) + mg(east)) / 2, (eg(west) + eg(east)) / 2);

                result.push(v);
            }
        }

        result
    }

    fn gradients(
        &self,
        _parameters: &[(f32, f32)],
        trace: &[i8],
        f: f32,
        phase: f32,
        sf: f32,
        g: &mut [(f32, f32)],
    ) {
        g.iter_mut().zip(trace).for_each(|(g, t)| {
            let t = *t as f32;
            g.0 += t * f * phase / 62.;
            g.1 += sf * t * f * (62. - phase) / 62.;
        });
    }

    fn constraints(&self, parameters: &mut [(f32, f32)]) {
        // This isn't the cleanest solution
        let (from, to) = if self.name == "PAWN_PST" {
            (4, 28)
        } else {
            (0, 32)
        };

        let parameters = &mut parameters[from..to];

        let total = parameters.iter().fold((0., 0.), |total, current| {
            (total.0 + current.0, total.1 + current.1)
        });

        parameters.iter_mut().for_each(|p| {
            p.0 -= total.0 / (to - from) as f32;
            p.1 -= total.1 / (to - from) as f32;
        });
    }

    fn print(&self, values: &[(f32, f32)]) {
        println!("#[rustfmt::skip]");
        println!(
            "pub static {name}: SquareMap<EScore> = SquareMap::visual([",
            name = self.name
        );
        for rank in Rank::all().into_iter().rev() {
            print!("    ");
            for file in 0..8 {
                let file = std::cmp::min(file, 7 - file);
                let file = File::new(file).unwrap();
                let v = values[rank as usize * 4 + file as usize];
                print!("S({:>4}, {:>4}), ", v.0 as Score, v.1 as Score);
            }
            println!();
        }
        println!("]);");
    }
}

#[derive(Clone, Debug)]
pub struct Array<const ENABLED: bool, const N: usize> {
    pub inner: [[i8; N]; 2],
    name: &'static str,
    source: &'static [EScore; N],
}

impl<const ENABLED: bool, const N: usize> Array<ENABLED, N> {
    fn new(name: &'static str, source: &'static [EScore; N]) -> Self {
        Self {
            inner: [[0; N]; 2],
            name,
            source,
        }
    }
}

pub struct ArraySchema<const N: usize> {
    name: &'static str,
    source: &'static [EScore; N],
}

impl<const ENABLED: bool, const N: usize> Tunable for Array<ENABLED, N> {
    fn enabled(&self) -> bool {
        ENABLED
    }

    fn compact(&self) -> Vec<i8> {
        let w = Side::White as usize;
        let b = Side::Black as usize;
        let mut result = Vec::with_capacity(N);
        for i in 0..N {
            result.push(self.inner[w][i] - self.inner[b][i]);
        }

        result
    }

    fn schema(&self) -> Box<dyn Schema> {
        Box::new(ArraySchema {
            name: self.name,
            source: self.source,
        })
    }
}

impl<const N: usize> Schema for ArraySchema<N> {
    fn num_parameters(&self) -> usize {
        N
    }

    fn initial_parameter_values(&self) -> Vec<EScore> {
        self.source.iter().copied().collect()
    }

    fn gradients(
        &self,
        _parameters: &[(f32, f32)],
        trace: &[i8],
        f: f32,
        phase: f32,
        sf: f32,
        g: &mut [(f32, f32)],
    ) {
        g.iter_mut().zip(trace).for_each(|(g, t)| {
            let t = *t as f32;
            g.0 += t * f * phase / 62.;
            g.1 += sf * t * f * (62. - phase) / 62.;
        });
    }

    fn constraints(&self, _parameters: &mut [(f32, f32)]) {}

    fn print(&self, values: &[(f32, f32)]) {
        println!("#[rustfmt::skip]");
        println!("pub static {name}: [EScore; {N}] = [", name = self.name);
        for i in 0..N {
            if i % 8 == 0 {
                if i > 0 {
                    println!();
                }
                print!("    ");
            }
            print!(
                "S({:>4}, {:>4}), ",
                values[i].0 as Score, values[i].1 as Score
            );
        }
        println!();
        println!("];");
    }
}
pub struct Tuner {
    k: f32,

    schemas: Schemas,
    parameters: Parameters,
    traces: Vec<CompactTrace>,
}

impl Tuner {
    pub fn new(positions: &[(Position, f32)]) -> Self {
        let schemas = Schemas::from(Trace::new(&positions[0].0, positions[0].1));
        let parameters = Parameters::from(&schemas);

        let traces = positions
            .iter()
            .map(|(position, result)| {
                let trace = Trace::new(position, *result);
                CompactTrace::new(&trace, &parameters)
            })
            .collect::<Vec<_>>();

        println!("Computing optimal k factor...");
        let k = Self::compute_optimal_k(&parameters, &traces);
        println!("Done. k = {k}");

        Self {
            k,
            schemas,
            parameters,
            traces,
        }
    }

    pub fn print(&self) {
        for (schema, offset) in &self.schemas.schemas {
            let len = schema.num_parameters();
            let params = &self.parameters.parameters[*offset..*offset + len];
            println!();
            schema.print(params);
        }
    }

    pub fn total_error(&self) -> f32 {
        Self::total_error_impl(&self.parameters, &self.traces, self.k)
    }

    pub fn shuffle_traces(&mut self, rng: &mut Xoshiro) {
        for _ in 0..self.traces.len() {
            let a = rng.gen() as usize % self.traces.len();
            let b = rng.gen() as usize % self.traces.len();

            self.traces.swap(a, b);
        }
    }

    fn total_error_impl(parameters: &Parameters, traces: &[CompactTrace], k: f32) -> f32 {
        let mut total = 0.;
        let n = traces.len() as f32;

        for trace in traces {
            total += (trace.result - sigmoid(k, parameters.eval(trace))).powf(2.);
        }

        total / n
    }

    fn compute_optimal_k(parameters: &Parameters, traces: &[CompactTrace]) -> f32 {
        let mut delta = 1.;
        let mut best = 1.;
        let mut best_error = Self::total_error_impl(parameters, traces, best);
        let mut low = best - delta;

        for _ in 0..5 {
            let step = delta / 5.;

            for i in 0..11 {
                let candidate = low + step * i as f32;
                let error = Self::total_error_impl(parameters, traces, candidate);

                if error < best_error {
                    best = candidate;
                    best_error = error;
                }
            }

            delta /= 10.;
            low = best - delta;
        }

        best
    }

    pub fn step(&mut self, f: f32) {
        const BATCH_SIZE: usize = 2048;
        for batch in self.traces.chunks(BATCH_SIZE) {
            Self::batch_step(&mut self.parameters, batch, &self.schemas, self.k, f);
        }
    }

    fn batch_step(
        parameters: &mut Parameters,
        traces: &[CompactTrace],
        schemas: &Schemas,
        k: f32,
        f: f32,
    ) {
        let mut g = vec![(0., 0.); parameters.parameters.len()];
        let n = traces.len();

        for trace in traces {
            let phase = trace.phase;
            let sf = trace.sf;
            let r = trace.result;
            let s = sigmoid(k, parameters.eval(trace));
            let grad = -(r - s) * s * (1. - s);
            if grad == 0. {
                continue;
            }

            for (schema, offset) in &schemas.schemas {
                let len = schema.num_parameters();
                let parameters = &parameters.parameters[*offset..*offset + len];
                let trace = &trace.data[*offset..*offset + len];
                let g = &mut g[*offset..*offset + len];
                schema.gradients(parameters, trace, grad, phase, sf, g);
            }
        }

        //let norm = norm(&g);
        //if norm == 0. {
        //return;
        //}
        //let norm = norm.sqrt();
        //let f = f / norm;

        parameters.parameters.iter_mut().zip(&g).for_each(|(p, g)| {
            p.0 -= 2. * f * g.0 / n as f32;
            p.1 -= 2. * f * g.1 / n as f32;
        });

        for (schema, offset) in &schemas.schemas {
            let len = schema.num_parameters();
            let parameters = &mut parameters.parameters[*offset..*offset + len];
            schema.constraints(parameters);
        }
    }
}

fn sigmoid(k: f32, q: f32) -> f32 {
    1. / (1. + 10_f32.powf(-k * q / 400.))
}

fn norm(grad: &[(f32, f32)]) -> f32 {
    grad.iter().flat_map(|x| [x.0, x.1]).map(|x| x * x).sum()
}
