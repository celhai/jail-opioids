#!/usr/bin/env python3
import random
import math
class User:
    def __init__(self, add, risky, tol, jail, time, loc):
        self.add = add
        self.risky = risky
        self.tol = tol
        self.jail = jail
        self.time = time if jail else 0
        self.loc = loc
    def jailed():
        self.jail = False if self.jail else True
        self.time = 0
    def timeIn(incr):
        self.time = self.time + incr if self.time < 14 else self.jailed()
        self.add -= 0.01*incr
        self.tol -= 0.02*incr
    def getLocation():
        return self.loc
    def move(area, incr):


    def passTime(grid, incr):
        self.risky += 0.02*incr
        self.add -= 0.01*incr
        self.tol -= 0.02*incr
        if !self.jail and opisAvail(self.loc, self.risky):
            return self.doOpis(incr)
        else:
            return False

    def doOpis(incr):
        self.add += 0.05*incr
        self.tol += 0.05*incr
        randNum = random.random()
        if randNum < self.risky
            self.jailed()
            self.risky = 0
        return True
    def getStatus():
        return [self.add, self.risky, self.tol, self.jail, self.time]

def opisAvail(location):


def canGetOps(location, riskyness):
