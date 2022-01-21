#ifndef _VIPON_DELAYIMP_H
#define _VIPON_DELAYIMP_H

typedef struct ImgDelayDescr {
    DWORD grAttrs;
    RVA rvaDLLName;
    RVA rvaHmod;
    RVA rvaIAT;
    RVA rvaINT;
    RVA rvaBoundIAT;
    RVA rvaUnloadIAT;
    DWORD dwTimeStamp;
} ImgDelayDescr,*PImgDelayDescr;

#endif /* _VIPON_DELAYIMP_H */

