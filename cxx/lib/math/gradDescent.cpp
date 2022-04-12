#include "comdef.h"
#include "gradDescent.h"

std::pair<double, Vector>
gradDescent(
    CostFunc costFunc,
    const Matrix& X,
    const Vector& y,
    const Vector& initTheta,
    size_t numIter,
    double regParam,
    double alpha
)
{
    Vector theta(initTheta);
    std::pair<double, Vector> res;
    for (size_t i = 0; i < numIter; ++i)
    {
        res = costFunc(X, y, theta, regParam);
        //std::cout << res.first << std::endl;
        //std::cout << res.second << std::endl;
        theta -= alpha * res.second;
        //std::cout << theta << std::endl;
    }

    res.second = theta;
    return res;
}

