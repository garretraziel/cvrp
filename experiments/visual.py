#!/usr/bin/env python2

import sys
import pygame
import random

pygame.init()

window = pygame.display.set_mode((500,500))

points = {1:(82,76), 2: (96, 44), 3: (50, 5), 4: (49, 8), 5: (13, 7), 6: (29, 89), 7: (58, 30), 8: (84, 39), 9: (14, 24), 10: (2, 39), 11: (3, 82), 12: (5, 10), 13: (98, 52), 14: (84, 25), 15: (61, 59), 16: (1, 65), 17: (88, 51), 18: (91, 2), 19: (19, 32), 20: (93, 3), 21: (50, 93), 22: (98, 14), 23: (5, 42), 24: (42, 9), 25: (61, 62), 26: (9, 97), 27: (80, 55), 28: (57, 69), 29: (23, 15), 30: (20, 70), 31: (85, 60), 32: (98, 5)} # n32

#points = {1: (86, 22), 2: (29, 17), 3: (4, 50), 4: (25, 13), 5: (67, 37), 6: (13, 7), 7: (62, 15), 8: (84, 38), 9: (34, 3), 10: (19, 45), 11: (42, 76), 12: (40, 86), 13: (25, 94), 14: (63, 57), 15: (75, 24), 16: (61, 85), 17: (87, 38), 18: (54, 39), 19: (66, 34), 20: (46, 39), 21: (47, 17), 22: (21, 54), 23: (19, 83), 24: (1, 82), 25: (94, 28), 26: (82, 72), 27: (41, 59), 28: (100, 77), 29: (1, 57), 30: (96, 7), 31: (57, 82), 32: (47, 38), 33: (68, 89), 34: (16, 36), 35: (51, 38), 36: (83, 74), 37: (84, 2)} # n37

# ways = [[7,24,29,5,12,9,10,19,15],
#        [4,3,14,8,2,13],
#        [28,25],
#        [27,18,20,32,22,17,31],
#        [21,30,23,16,11,26,6]]

# ways = [[21,30,9,12,5,29,7,25],
#        [15,18,20,32,22,2,13],
#        [28,19,10,23,16,11,26,6],
#        [31,27,17,8,14,3,4,24]]

ways = [[24,3,21,10,16,11],
       [23,31,27,4,7,15,19,30,25],
       [8,18,5,32,20],
       [17,13,26,6],
       [29,9,12,14,28,2,22]]

ways = [[29,12,26,6,21],
       [19,9,3,32,2,15,17],
       [13,8,22,20,18],
       [4,5,25,14,24,7],
       [30,11,16,10,23,28,27,31]]

ways = [[20,32,18,3,24,29,15],
       [13,27,17,31,14,8,2],
       [28,6,16,11,30,26,25],
       [21,7,19,5,9,23,10,12,22,4]]

ways = [[17,13,18,20,32,27],
       [25],
       [21,6,30,11,16,19,9,29,22,2],
       [28,15,5,12,10,23,26],
       [31,8,14,3,4,24,7]]

ways = [[29,5,12,9,19,28],
       [25,15,7,24,4,3,8],
       [14,18,20,32,22,2],
       [30,23,10,16,11,26,6,21],
       [13,17,27,31]]

ways = [[25,15,7,14,8],
       [23,10,19,9,12,5,29,3],
       [24,4,18,20,32,22,2],
       [28,30,16,11,26,6,21],
       [31,27,17,13]]

ways = [[13,2,17,31],
       [21,6,26,11,28],
       [8,14,18,20,32,22],
       [29,5,12,9,19,10,23,16,30,15],
       [27,7,4,3,24,25]]

# n37
ways = [[26,31,27,32,35,5],
       [25,17,8,15,37,30],
       [19,21,2,4,34,10,24,23,13,20],
       [7,9,6,3,29,22],
       [14,36],
       [28,33,16,12,11,18]]

ways = [[19,5,13,12,31,16,33],
       [18,22,3,29,24,23,11],
       [27,10,34,6,4,2,9],
       [14,35,20,32,21,7],
       [28,36,26,8],
       [30,37,15,17,25]] # n37 fitness: 1013

ways = [[14,27,22,10,34,2],
       [17,36,26,8],
       [7,21,9,37,30,25],
       [12,13,23,24,29,3,6,4],
       [28,33,16,31,11,5],
       [19,18,35,20,32,15]] # n37 fitness: 999

# n32
# ways = [[12,5,29,24,3,4,7],
#        [21,30,19,9,10,23,16,11,26,6],
#        [28,25,15],
#        [8,22,32,20,18,14,27],
#        [13,2,17,31]] # n32, 803

# ways = [[6,26,11,16,23,10,19,29],
#        [31,17,2,13],
#        [25,28],
#        [15,7,4,3,24,5,12,9,30,21],
#        [27,8,14,18,20,32,22]] # n32, fitness: 813

# ways = [[21,6,26,11,16,10,23,30],
#        [19,9,12,5,29,24,3,4,7],
#        [15,25,28],
#        [31,17,2,13],
#        [27,8,14,18,20,32,22]] # n32, fitness: 782 ??

# ways = [[26,11,30,9,12,5,29],
#        [27,8,3,4,24,7,28],
#        [13,17,31],
#        [25,15,19,10,23,16,6,21],
#        [14,18,20,32,22,2]] # n32, 910

ways = [[31,27,7,24,29,5,12,9,19,15],
       [30,23,10,16,11,26,6,21],
       [25,28],
       [17,8,14,2,13],
       [22,32,20,18,3,4]] # n32, fitness: 836

ways = [[15,23,10,19,9,12,5,29,24,7],
       [13,2,22,32,14,8],
       [25,4,3,18,20],
       [28,30,16,11,26,6,21],
       [31,17,27]] # n32, jine koeficienty, fitness: 858

ways = [[21,6,26,11,23,10,12,5],
       [31,17,2,13],
       [27,8,14,18,20,32,22],
       [25,28],
       [15,7,4,3,24,29,9,19,16,30]] # n32, mensi pocet jedincu, fitness: 817

ways = [[28,33,31,14,19],
       [8,36,26,17],
       [18,32,20,27,12,16],
       [5,35,10,6,4,2,9],
       [21,34,3,22,29,24,23,13,11],
       [25,30,37,7,15]] # n32, mensi populace, fitness: 1042

# ways = [[21,6,26,11,16,23,10,9,19,30],
#        [27,8,22,32,20,18,14],
#        [31,17,2,13],
#        [28,25],
#        [15,7,29,12,5,24,3,4]] # n32, standard, 797

# ways = [[31,17,2,13],
#        [7,4,3,24,29,5,12,19,15],
#        [27,8,22,32,20,18,14],
#        [28,25],
#        [21,6,26,11,30,16,23,10,9]] # n32, standard, 790

#ways = [[21, 31, 19, 17, 13, 7, 26], [12, 1, 16, 30], [27, 24], [29, 18, 8, 9, 22, 15, 10, 25, 5, 20], [14, 28, 11, 4, 23, 3, 2, 6]]

#ways = [[16,31,12,11,27],
#       [5,22,10,34,3,29,24,23,13],
#       [17,8,2,4,6,9,21],
#       [14,20,32,35,18,19],
#       [15,7,37,30,25],
#       [28,33,36,26]] # n37, fitness: 995

ways = [[15,25,28],
       [8,22,32,20,18,14,27],
       [21,6,26,11,16,10,23,30],
       [31,17,2,13],
       [7,4,3,24,29,5,12,9,19]] # n32, mutace 1, 2 tahy, prekroceni kapacity fitness: 784

# ways = [[21,30,19,9,10,23,16,11,26,6],
#        [7,4,3,24,5,12,29,15],
#        [27,8,14,18,20,32,22],
#        [28,25],
#        [13,2,17,31]] # fitness: 795

for i, car in enumerate(ways):
    color = (random.randrange(255), random.randrange(255), random.randrange(255))
    prev = 1
    for city in car:
        x1, y1 = points[prev]
        x2, y2 = points[city]
        pygame.draw.line(window, color, (x1*5, 500-y1*5), (x2*5, 500-y2*5))
        prev = city
    x1, y1 = points[prev]
    x2, y2 = points[1]
    pygame.draw.line(window, color, (x1*5, 500-y1*5), (x2*5, 500-y2*5))

pygame.display.flip()

while True: 
   for event in pygame.event.get(): 
      if event.type == pygame.QUIT: 
          sys.exit(0) 