bruto_neto <- function(x) {
        # Tax.lt skaičiuoklė
        # https://www.tax.lt/skaiciuokles/atlyginimo_ir_mokesciu_skaiciuokle
        
        # GPM įstyatymas
        # https://e-seimas.lrs.lt/portal/legalAct/lt/TAD/TAIS.171369/asr

        # VMI
        # https://www.vmi.lt/cms/documents/10174/8274962/KD-8860+tarifai+nuo+2019+met%C5%B3/f4d12f92-cb8c-4c5a-bd59-f9317c9507d9
        
        # Baziniai dydžiai
        # https://www.sodra.lt/lt/situacijos/statistika/pagrindiniai-socialiniai-rodikliai
        
        # 2019 prog VDU
        # http://finmin.lrv.lt/lt/aktualus-valstybes-finansu-duomenys/ekonomines-raidos-scenarijus
        
        
        # Baziniai parametrai
        GPM_1 <- 0.20
        GPM_2 <- 0.27
        PSD <- 0.0698
        SODRA <- 0.1252
        MMA <- 555
        VDU <- 1283.2
        lubos <- 10*VDU
        NPD <- 300
        NPD_coef <- 0.15
        
        
        bruto <- x
        
        # GPM skaičiavimas
        
        #NPD formulė: 
        #NPD = 300 – 0,15 x (bruto -MMA).
        npd <- max(NPD - NPD_coef* max(0,(bruto - MMA)),0)
        
        # mokestinė bazė GPM'ui
        mok_baz <- max(0,(bruto-npd))
        
        # gyventojo GPM
        gpm <- ifelse(bruto<=lubos, mok_baz*GPM_1, lubos *GPM_1+(bruto-lubos)*GPM_2) 
        
        # darbuotojo socialinio draudimo įmokos
        sodra <- min(bruto*SODRA, lubos * SODRA)
        
        # darbuotojo sveikatos draudimo įmokos
        psd <- bruto*PSD
        
        neto <- bruto - gpm - sodra - psd
        neto
}