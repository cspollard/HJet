#define TI_cxx
#include "TI.h"
#include <iostream>
#include <cmath>
#include <unistd.h>
#include <TMath.h>


using namespace std;

void TI::Loop() {
    if (fChain == 0) return;

    Long64_t nentries = fChain->GetEntriesFast();

    Long64_t nbytes = 0, nb = 0;

    float clx, cly, clz, clt;
    for (Long64_t jentry=0; jentry<nentries;jentry++) {
        Long64_t ientry = LoadTree(jentry);
        if (ientry < 0) break;
        nb = fChain->GetEntry(jentry);   nbytes += nb;

        cerr << endl;
        cerr << "actual jets:" << endl;
        for (int i = 0; i < jet_n; i++) {
            cerr << "(" << jet_pt->at(i) << "," << jet_eta->at(i)
                << "," << jet_phi->at(i) << "," <<
                jet_E->at(i) << ")" << endl;
        }
        cerr << endl;

        /*
        cerr << "cl_n: " << cl_n << endl;
        for (int i = 0; i < cl_n; i++) {
            if (cl_pt->at(i) < 20000)
                continue;

            cerr << "(" << cl_pt->at(i) << "," << cl_eta->at(i) << "," <<
                cl_phi->at(i) << "," << cl_pt->at(i)*TMath::CosH(cl_eta->at(i)) << ")" << endl;
        }

        for (int i = 0; i < cl_n; i++) {
            if (cl_pt->at(i) < 20000)
                continue;

            clx = cl_pt->at(i)*cos(cl_phi->at(i));
            cly = cl_pt->at(i)*sin(cl_phi->at(i));
            clz = cl_pt->at(i)*sinh(cl_eta->at(i));
            clt = cl_pt->at(i)*cosh(cl_eta->at(i));

            cerr << "(" << clx << "," << cly << "," << clz << "," << clt << ")" << endl;
        }
        */

        cout << "<event>" << endl;
        for (int i = 0; i < cl_n; i++) {
            if (cl_pt->at(i) < 500)
                continue;

            cout << "(" << cl_pt->at(i) << "," << cl_eta->at(i) << "," <<
                cl_phi->at(i) << "," << cl_pt->at(i)*TMath::CosH(cl_eta->at(i)) << ")" << endl;

        }
        cout << "</event>" << endl;

        return;
    }
}
