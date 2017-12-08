#pragma once

// TODO: I feel a little bad putting constants in the global namespace, maybe
// it's time to add a namespace for the whole project

static const int MAX_PLY = 100;
static const int NUM_KILLER_MOVE_SLOTS = 2;

// Scaled to fit within a 16-bit signed int
static const int SCORE_DRAW = 0;
static const int SCORE_MATE = 30000;
static const int SCORE_MIN_MATE = SCORE_MATE - MAX_PLY;
static const int SCORE_INF = 30001;