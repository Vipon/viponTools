#include "normalEquation.h"

Matrix normalEquation(Matrix X, Matrix Y)
{
    return (X.transpone() * X).pinv() * X.transpone() * Y;
}

