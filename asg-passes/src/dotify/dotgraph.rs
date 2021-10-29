// Copyright (C) 2019-2021 Aleo Systems Inc.
// This file is part of the Leo library.

// The Leo library is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// The Leo library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with the Leo library. If not, see <https://www.gnu.org/licenses/>.

use petgraph::graph::{EdgeIndex, Graph, NodeIndex};
use petgraph::visit::{DfsPostOrder, EdgeRef};
use petgraph::Direction;
use std::borrow::Cow;

pub struct DotNode {
    pub id: String,
    pub name: String,
    pub labels: Vec<(&'static str, String)>,
}

impl DotNode {
    pub fn filter_labels(&mut self, excluded_labels: &[String]) {
        self.labels = self
            .labels
            .drain(..)
            .filter(|(key, _)| excluded_labels.contains(&String::from(*key)))
            .collect();
    }
}

pub struct DotEdge {
    pub start_idx: NodeIndex,
    pub end_idx: NodeIndex,
    pub label: String,
    pub color: &'static str,
}

pub struct DotGraph {
    id: String,
    graph: Graph<DotNode, DotEdge>,
    source: NodeIndex,
}

impl DotGraph {
    pub fn new(id: String) -> Self {
        DotGraph {
            id,
            graph: Graph::new(),
            source: NodeIndex::default(),
        }
    }

    pub fn get_source(&self) -> NodeIndex {
        self.source
    }

    pub fn set_source(&mut self, idx: NodeIndex) {
        self.source = idx;
    }

    pub fn add_node(&mut self, node: DotNode) -> NodeIndex {
        self.graph.add_node(node)
    }

    pub fn add_edge(&mut self, edge: DotEdge) -> EdgeIndex {
        // Prevents duplicate edges as traversals may go through paths multiple times
        self.graph.update_edge(edge.start_idx, edge.end_idx, edge)
    }

    pub fn filter_node_labels(&mut self, excluded_labels: &[String]) {
        for node in self.graph.node_weights_mut() {
            node.filter_labels(excluded_labels)
        }
    }

    pub fn filter_node_edges(&mut self, excluded_edges: &[String]) {
        self.graph.retain_edges(|graph, edge_idx| {
            let edge = &graph[edge_idx];
            !excluded_edges.contains(&edge.label)
        });
    }

    //todo: implement caching
    pub fn get_reachable_set(&self) -> Vec<NodeIndex> {
        let mut dfs = DfsPostOrder::new(&self.graph, self.source);
        let mut idxs = Vec::new();
        while let Some(idx) = dfs.next(&self.graph) {
            idxs.push(idx)
        }
        idxs
    }
}

impl<'a> dot::Labeller<'a, (NodeIndex, &'a DotNode), (EdgeIndex, &'a DotEdge)> for DotGraph {
    fn graph_id(&'a self) -> dot::Id<'a> {
        dot::Id::new(self.id.as_str()).unwrap()
    }

    fn node_id(&'a self, n: &(NodeIndex, &'a DotNode)) -> dot::Id<'a> {
        let &(i, _) = n;
        dot::Id::new(self.graph[i].id.as_str()).unwrap()
    }

    fn node_label(&'a self, n: &(NodeIndex, &'a DotNode)) -> dot::LabelText<'a> {
        let mut label = n.1.name.clone();
        for (key, value) in &n.1.labels {
            label.push_str(format!("\n{:}: {:}", key, value).as_str())
        }
        dot::LabelText::escaped(label)
    }

    fn edge_label(&'a self, e: &(EdgeIndex, &'a DotEdge)) -> dot::LabelText<'a> {
        dot::LabelText::label(e.1.label.as_str())
    }

    fn edge_end_arrow(&'a self, _e: &(EdgeIndex, &'a DotEdge)) -> dot::Arrow {
        dot::Arrow::from_arrow(dot::ArrowShape::Normal(dot::Fill::Filled, dot::Side::Both))
    }

    fn edge_color(&'a self, e: &(EdgeIndex, &'a DotEdge)) -> Option<dot::LabelText<'a>> {
        Some(dot::LabelText::label(e.1.color))
    }
}

impl<'a> dot::GraphWalk<'a, (NodeIndex, &'a DotNode), (EdgeIndex, &'a DotEdge)> for DotGraph {
    fn nodes(&'a self) -> dot::Nodes<'a, (NodeIndex, &'a DotNode)> {
        let mut dot_nodes = Vec::new();
        for idx in self.get_reachable_set() {
            dot_nodes.push((idx, &self.graph[idx]))
        }
        Cow::Owned(dot_nodes)
    }

    fn edges(&'a self) -> dot::Edges<'a, (EdgeIndex, &'a DotEdge)> {
        let mut dot_edges = Vec::new();
        for idx in self.get_reachable_set() {
            for edge in self.graph.edges_directed(idx, Direction::Outgoing) {
                dot_edges.push((edge.id(), edge.weight()));
            }
        }
        Cow::Owned(dot_edges)
    }

    fn source(&'a self, e: &(EdgeIndex, &'a DotEdge)) -> (NodeIndex, &'a DotNode) {
        let &(_, edge) = e;
        (edge.start_idx, &self.graph[edge.start_idx])
    }

    fn target(&'a self, e: &(EdgeIndex, &'a DotEdge)) -> (NodeIndex, &'a DotNode) {
        let &(_, edge) = e;
        (edge.end_idx, &self.graph[edge.end_idx])
    }
}
