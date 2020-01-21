#include "ac3.h"

bool revise (csp* CSP, std::string Xi, std::string Xj) {
    bool revised = false;

    std::set <int> XiDom = CSP->getVariableDomain().at(Xi);
    std::set <int> XjDom = CSP->getVariableDomain().at(Xj);

    std::set <int>::iterator iterXi;
    std::set <int>::iterator iterXj;

    for (iterXi = XiDom.begin(); iterXi != XiDom.end(); ++iterXi) { 
        bool valid = false;
        int x = *iterXi;
        for (iterXj = XiDom.begin(); iterXj != XiDom.end(); ++iterXj) { 
            int y = *iterXj;

            //constraint satisfaction
            if (x != y) {
                valid = true;
            }
        }
        
        //if constaint is not satisfied, remove value from domain
        if (!valid) {
            revised = true;
            if ((XiDom.size() != 1) && (XjDom.size() != 1)) {
                CSP->removeValFromDomain(CSP, Xi, x);
            }
            else if ((XiDom.size() == 1) && (XjDom.size() != 1)) {
                CSP->removeValFromDomain(CSP, Xj, x);
            }
            else if ((XiDom.size() != 1) && (XjDom.size() == 1)) {
                CSP->removeValFromDomain(CSP, Xi, x);
            }
            else {
                revised = false;
            }
        }
    }
    return revised;
}

bool ac3 (csp *CSP) {
    std::queue <std::pair <std::string, std::string> > qArcs = CSP->setArcs();

    while (!qArcs.empty()) {
        //get first arc in queue
        std::pair <std::string, std::string> arc = qArcs.front();
        qArcs.pop();

        std::string Xi = arc.first;
        std::string Xj = arc.second;

        if (Xi != Xj) {
            if (revise(CSP, Xi, Xj)) {
                std::set <int> xDom = CSP->getVariableDomain().at(Xi);

                if (xDom.empty()) {
                    return false;
                }
                else {
                    std::set <std::string> allNeighbors = CSP->getNeighbors(CSP, Xi, Xj);

                    while (allNeighbors.empty() != true) {
                        std::set <std::string>::iterator iter = allNeighbors.begin();
                        std::string Xk;

                        if (iter != allNeighbors.end()) {
                            Xk = *iter;
                            qArcs.push(std::make_pair(Xk, Xi));
                            allNeighbors.erase(iter);
                        }
                    }
                }
            }
        } 
    }
    return true;
}
