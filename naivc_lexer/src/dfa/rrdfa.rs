use std::{
    collections::{BTreeSet, HashMap},
    rc::Rc,
};

use crate::dfa::{
    DFAState, DFAStateHandle, DeterministicFiniteAutomaton, LabelledTransition, PriorizedTokenType,
    RangePartitioner,
    rdfa::{RDFAStateHandle, ReversedRDFAState},
};

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash)]
struct RRDFAStateHandle(usize);

impl RRDFAStateHandle {
    pub fn new(index: usize) -> Self {
        Self(index)
    }

    pub fn index(&self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct RDFATrackableStateHandle {
    start: BTreeSet<PriorizedTokenType>,
    state: RDFAStateHandle,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
struct RDFAStateSet {
    states: BTreeSet<RDFATrackableStateHandle>,
}

#[derive(Debug, Clone)]
pub struct RRDFAState {
    rdfa_states: Rc<RDFAStateSet>,
    transitions: BTreeSet<LabelledTransition<RRDFAStateHandle>>,
    pub bind_patterns: BTreeSet<PriorizedTokenType>,
    pub accept_patterns: BTreeSet<PriorizedTokenType>,
}

impl RRDFAState {
    pub fn iter_sub_states(&self) -> impl Iterator<Item = &RDFATrackableStateHandle> {
        self.rdfa_states.states.iter()
    }
}

#[derive(Debug, Clone)]
pub struct RRDeterministicFiniteAutomaton {
    rdfa_states: HashMap<RDFAStateHandle, ReversedRDFAState>,
    start_states: BTreeSet<RDFAStateHandle>,
    accept_state: RDFAStateHandle,
}

impl RRDeterministicFiniteAutomaton {
    pub fn new(
        rdfa_states: HashMap<RDFAStateHandle, ReversedRDFAState>,
        start_states: BTreeSet<RDFAStateHandle>,
        accept_state: RDFAStateHandle,
    ) -> Self {
        Self {
            rdfa_states,
            start_states,
            accept_state,
        }
    }

    pub fn subset_reachable(self) -> DeterministicFiniteAutomaton {
        let builder = SubsetDFABuilder::new(self);
        let subset_dfa = builder.build();
        subset_dfa.to_reachable_dfa()
    }
}

#[derive(Debug, Clone)]
struct SubsetDFA {
    states: Vec<RRDFAState>,
    start_state: RRDFAStateHandle,
}

impl SubsetDFA {
    fn find_reachable_states(&self) -> BTreeSet<RRDFAStateHandle> {
        let mut visited = BTreeSet::new();
        let mut stack = vec![self.start_state];

        while let Some(current) = stack.pop() {
            if visited.insert(current) {
                let state = &self.states[current.index()];
                for transition in &state.transitions {
                    stack.push(transition.target);
                }
            }
        }
        visited
    }

    fn to_reachable_dfa(self) -> DeterministicFiniteAutomaton {
        let reachable = self.find_reachable_states();

        let handle_map = reachable
            .iter()
            .enumerate()
            .map(|(new_idx, &old_handle)| (old_handle, DFAStateHandle(new_idx)))
            .collect::<HashMap<_, _>>();

        let dfa_states = reachable
            .iter()
            .map(|&old_handle| {
                let state = &self.states[old_handle.index()];
                let transitions = state
                    .transitions
                    .iter()
                    .map(|transition| LabelledTransition {
                        range: transition.range.clone(),
                        target: handle_map[&transition.target],
                    })
                    .collect::<BTreeSet<_>>();
                let bind_patterns = state.bind_patterns.clone();
                let accept_patterns = state.accept_patterns.clone();
                let accept_pattern = accept_patterns
                    .iter()
                    .min()
                    .map(|pattern| pattern.token_type.clone());
                DFAState {
                    transitions,
                    bind_patterns,
                    accept_patterns,
                    accept_pattern,
                }
            })
            .collect();

        DeterministicFiniteAutomaton {
            dfa_states,
            start_state: handle_map[&self.start_state],
        }
    }
}

#[derive(Debug, Clone)]
struct PatternSet {
    bind: BTreeSet<PriorizedTokenType>,
    accept: BTreeSet<PriorizedTokenType>,
}

impl PatternSet {
    fn new() -> Self {
        Self {
            bind: BTreeSet::new(),
            accept: BTreeSet::new(),
        }
    }
}

struct SubsetDFABuilder {
    rdfa: RRDeterministicFiniteAutomaton,
    subset_map: HashMap<Rc<RDFAStateSet>, RRDFAStateHandle>,
    states: Vec<RRDFAState>,
    worklist: Vec<RRDFAStateHandle>,
}

impl SubsetDFABuilder {
    fn new(rdfa: RRDeterministicFiniteAutomaton) -> Self {
        Self {
            rdfa,
            subset_map: HashMap::new(),
            states: Vec::new(),
            worklist: Vec::new(),
        }
    }

    fn build(mut self) -> SubsetDFA {
        let start_state = self.create_start_state();
        self.process_worklist();

        SubsetDFA {
            states: self.states,
            start_state,
        }
    }

    fn create_start_state(&mut self) -> RRDFAStateHandle {
        let start_subset = Rc::new(RDFAStateSet {
            states: self
                .rdfa
                .start_states
                .iter()
                .filter_map(|state| {
                    if let Some(rdfa_state) = self.rdfa.rdfa_states.get(state) {
                        Some(RDFATrackableStateHandle {
                            start: rdfa_state.accept_patterns.clone(),
                            state: state.clone(),
                        })
                    } else {
                        None
                    }
                })
                .collect(),
        });

        let patterns = self.collect_start_patterns(&start_subset);
        let start_state = RRDFAState {
            rdfa_states: start_subset.clone(),
            transitions: BTreeSet::new(),
            bind_patterns: patterns.bind,
            accept_patterns: patterns.accept,
        };

        let handle = RRDFAStateHandle::new(0);
        self.subset_map.insert(start_subset, handle);
        self.states.push(start_state);
        self.worklist.push(handle);

        handle
    }

    fn collect_start_patterns(&self, subset: &Rc<RDFAStateSet>) -> PatternSet {
        let bind = subset
            .states
            .iter()
            .filter_map(|handle| self.rdfa.rdfa_states.get(&handle.state))
            .flat_map(|rdfa_state| rdfa_state.accept_patterns.iter().cloned())
            .collect();

        let accept = self
            .rdfa
            .rdfa_states
            .get(&self.rdfa.accept_state)
            .map(|state| state.accept_patterns.clone())
            .unwrap_or_default();

        PatternSet { bind, accept }
    }

    fn process_worklist(&mut self) {
        while let Some(current_handle) = self.worklist.pop() {
            self.process_state(current_handle);
        }
    }

    fn process_state(&mut self, handle: RRDFAStateHandle) {
        let current_state = self.states[handle.index()].clone();
        let partitioned_ranges = self.compute_partitioned_ranges(&current_state);

        for range in partitioned_ranges {
            if let Some((next_handle, patterns)) =
                self.compute_transition(&current_state, range.clone())
            {
                let state = &mut self.states[handle.index()];
                state.transitions.insert(LabelledTransition {
                    range: range.clone(),
                    target: next_handle,
                });
                let next_state = &mut self.states[next_handle.index()];
                next_state.bind_patterns.extend(patterns.bind);
                next_state.accept_patterns.extend(patterns.accept);
            }
        }
    }

    fn compute_partitioned_ranges(&self, state: &RRDFAState) -> RangePartitioner {
        let transition_set = state
            .iter_sub_states()
            .filter_map(|handle| self.rdfa.rdfa_states.get(&handle.state))
            .flat_map(|rdfa_state| {
                rdfa_state
                    .transitions
                    .iter()
                    .map(|transition| &transition.range)
            });
        // Since the reverse DFA may have overlapping transitions, we could not use SortedRangePartitioner here
        RangePartitioner::new(transition_set)
    }

    fn compute_transition(
        &mut self,
        current_state: &RRDFAState,
        range: std::ops::RangeInclusive<char>,
    ) -> Option<(RRDFAStateHandle, PatternSet)> {
        let mut next_subset = BTreeSet::new();
        let mut pattern_set = PatternSet::new();

        for sub_state_handle in current_state.iter_sub_states() {
            if let Some(rdfa_state) = self.rdfa.rdfa_states.get(&sub_state_handle.state) {
                for next_rdfa_state in rdfa_state.next_by(*range.start()) {
                    next_subset.insert(RDFATrackableStateHandle {
                        start: sub_state_handle.start.clone(),
                        state: next_rdfa_state,
                    });
                    pattern_set
                        .bind
                        .extend(sub_state_handle.start.clone());
                    if next_rdfa_state == self.rdfa.accept_state {
                        pattern_set
                            .accept
                            .extend(sub_state_handle.start.clone());
                    }
                }
            }
        }

        if next_subset.is_empty() {
            return None;
        }

        let next_subset_rc = Rc::new(RDFAStateSet {
            states: next_subset,
        });

        let next_handle = self.get_or_create_state(next_subset_rc);

        Some((
            next_handle,
            pattern_set,
        ))
    }

    fn get_or_create_state(&mut self, subset: Rc<RDFAStateSet>) -> RRDFAStateHandle {
        if let Some(&handle) = self.subset_map.get(&subset) {
            handle
        } else {
            let new_handle = RRDFAStateHandle::new(self.states.len());
            let new_state = RRDFAState {
                rdfa_states: subset.clone(),
                transitions: BTreeSet::new(),
                bind_patterns: BTreeSet::new(),
                accept_patterns: BTreeSet::new(),
            };
            self.states.push(new_state);
            self.subset_map.insert(subset, new_handle);
            self.worklist.push(new_handle);
            new_handle
        }
    }
}
