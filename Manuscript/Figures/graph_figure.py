#!/usr/bin/python3

import graph_tool.all as gt
import numpy as np
from matplotlib import pyplot as plt
import matplotlib.colors as colors
from random import randint
import copy

def make_graph():
   g = gt.Graph(directed = False)
   e = [[int(np.random.sample(1) * 10), int(np.random.sample(1) * 3)] for i in range(20)]
   g.add_edge_list(e)
   gt.remove_self_loops(g)
   gt.remove_parallel_edges(g)
   v = g.get_vertices()
   d = g.get_in_degrees(v) + g.get_out_degrees(v)
   to_remove = np.where(d == 0)
   if len(to_remove[0]) > 0:
	   g.remove_vertex(to_remove)
   return g

def scenarios_graphs(g):
   PN = copy.deepcopy(g)
   RE = copy.deepcopy(g)
   for i in range(2):
      RE.remove_vertex(randint(0, len(RE.get_vertices()) - 1))
   CU = copy.deepcopy(RE)
   for i in range(2):
      CU.remove_vertex(randint(0, len(CU.get_vertices()) - 1))
   PN = gt.minimize_nested_blockmodel_dl(PN)
   CU = gt.minimize_nested_blockmodel_dl(CU)
   RE = gt.minimize_nested_blockmodel_dl(RE)
   return([PN, CU, RE])

def figure(scenarios):
   PN = scenarios[0]
   CU = scenarios[1]
   RE = scenarios[2]
   PN.draw(vertex_size = 40, edge_pen_width = 10,
   vertex_pen_width = 0, vertex_fill_color = 'steelblue', 
   hide = True, output = "present-natural.svg")
   CU.draw(vertex_size = 40, edge_pen_width = 10,
   vertex_pen_width = 0, vertex_fill_color = 'steelblue', 
   hide = True, output = "current.svg")
   RE.draw(vertex_size = 40, edge_pen_width = 10,
   vertex_pen_width = 0, vertex_fill_color = 'steelblue', 
   hide = True, output = "rewilded.svg")

g = make_graph()
scenarios = scenarios_graphs(g)
figure(scenarios)
