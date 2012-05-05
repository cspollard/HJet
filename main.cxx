#include "TI.h"
#include "TFile.h"

int main(int argc, char *argv[]) {
    TFile *fin = new TFile(argv[1]);

    TTree *tin = (TTree *) fin->Get("physics");
    TI ti = TI(tin);

    ti.Loop();

    return 0;
}
