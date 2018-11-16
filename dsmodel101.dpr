library dsmodel101;
  {-Stikstofuitspoeling vlgs. 'Beschrijving SPREAD'.
    Ir. W. Beekman, KIWA 1998. Opdrachtnr. 11.1154.010 }

  { Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }
{.$define test}
{.$define test2}

uses
  ShareMem,
  {$ifdef test2} forms, {$endif} windows, SysUtils, Classes, LargeArrays,
  ExtParU, USpeedProc, uDCfunc, UdsModel, UdsModelS, xyTable, math, DUtils, uError;

Const
  cModelID      = 101;  {-Key to this model (=unique-ID)}

  {-Mapping of dependent variable vector (=aantal te integreren snelheden)}
  cNrOfDepVar   = 3;    {-Length of dependent variable vector}
  cNatGWaanv    = 1;    {-Natuurlijke grondwateraanvulling (m/d}
  cNuitsp       = 2;    {-Uitspoeling (kg N/ha/jr)}
  cNGewasOpn    = 3;    {-Gewasopname plus denitrificatie in wortelzone
                         (=(Nniveau - Un_niveau), kg N/ha/jr)}
						 
  {-Aantal keren dat een discontinuiteitsfunctie wordt aangeroepen in de procedure met
    snelheidsvergelijkingen (DerivsProc)}
  nDC = 0;

  {***** Used when booted for shell-usage ***}
  cnRP    = 8;  {-Nr. of RP-time-series that must be supplied by the shell in
                  EP[ indx-1 ].}
  cnSQ    = 0;  {-Nr. of point-time-series that must be supplied by the shell
                  in EP[ indx-1 ]. REM: point- sources niet aan de orde bij
                  stikstof-uitspoeling!}
  cnRQ    = 0;  {-Nr. of line-time-series that must be supplied
                  by the shell in EP[ indx-1 ]. REM: point- sources niet aan de
                  orde bij stikstof-uitspoeling!}

  {-Mapping of EP[cEP0]}
  cNrXIndepTblsInEP0 = 9;  {-Nr. of XIndep-tables in EP[cEP0]}
  cNrXdepTblsInEP0   = 0;  {-Nr. of Xdep-tables   in EP[cEP0]}

  cTbMinMaxValKeys  = 2;   {-EP[cEP0]: xIndep-Table numbering; 0&1 are reserved}
  cTbRelBasisUitsp  = 3;
  cTbHumusgehalte   = 4;
  cTb_a_b_max       = 5;
  cTbUitspFractWntr = 6;
  cTbCorrFactNuitsp = 7;
  cTbWeidPar        = 8;

  {-Mapping of EP[cEP1]: xdep-Table numbering}
  cTbNatGWaanv      = 0;
  cTbGt             = 1;
  cTbLandgebr       = 2;
  cTbBodemSrt       = 3;
  cTbKoeienPha      = 4;
  cTbNniveauZMR     = 5;
  cTbNniveauWNTR    = 6;
  cTbRefBasisNuitsp = 7;

  {-Landgebruik codes}
  cgras = 1; cbouwland = 2; cmais = 3; cnatuur = 4; cstedelijk = 5;

  {-Model specifieke fout-codes}
  cLandgebruikBodemsoortCombinatieOngeldig = -9001;
  cBasisuitspoelingOnbekend = -9002;
  cConstRelNuitspOnbekend = -9003;
  cUitspFractOnbekend = -9004;
  cInvalidGt = -9005;
  cInvalidLandgebr = -9006;
  cInvalidBodemSrt = -9007;
  cInvalidKoeienPha = -9008;
  cErrorCalcNurine = -9009;
  cErrCalcFUn_wz = -9010;
  cRefBasisUitspOngeldig = -9011;

var
  Indx: Integer; {-Index of Boot-procedure used. Must be set by boot-procedure!}
  {-Als verschillende TBootEPArray-functies de 'mapping' beinvloeden van
    de externe parameters op de EPArray, dan kan deze variabele binnen de
    Speed-procedures worden benut om toch de gewenste gegevens te vinden}
  ModelProfile: TModelProfile;
                 {-Object met met daarin de status van de discontinuiteitsfuncties
				   (zie nDC) }
  {-Min/max values of key-values: must be set by boot-procedure!}
  cGtMin, cGtMax,
  cLandgebrMin, cLandgebrMax,
  cBodemSrtMin, cBodemSrtMax,
  cKoeienPhaMin, cKoeienPhaMax: Integer;

Procedure MyDllProc( Reason: Integer );
begin
  if Reason = DLL_PROCESS_DETACH then begin {-DLL is unloading}
    {-Cleanup code here}
    if ( nDC > 0 ) then
      ModelProfile.Free;
  end;
end;

Procedure DerivsProc( var x: Double; var y, dydx: TLargeRealArray;
                      var EP: TExtParArray; var Direction: TDirection;
                      var Context: Tcontext; var aModelProfile: PModelProfile; var IErr: Integer );
{-Returns the derivatives dydx at location x, given, x, the function values y
  and the external parameters EP. IErr=0 if no error occured during the
  calculation of dydx}
var
  fDn,             {-Corr. fact. nitraatuitspoeling}
  Unbasis,      {-Basisuitspoeling (kg N/ha/jr), verg. 1 p. 9}
  Un_niveau,    {-De uitspoeling van stikstof (N) die plaatvindt in het groei-
                  seizoen op basis van het stikstofniveau i.d. wortelzone
                  (=Nniveau) en evt. de uitspoeling van weidemest (kg N/ha/jr).
                  p. 10-12 en 13-14. }
  Un_winter,    {-De winteruitspoeling; wordt berekend als fractie van de
                  winterbelasting (kg N/ha/jr)}
  Nniveau       {-De minerale stikstof die in het groeiseizoen voor plantopname
                {-beschikbaar is (=het stikstofniveau in de wortelzone, kg/ha/jr).
                {-Form. 2 p. 10}

  : Double;
  Gt, LandGebr, BodemSrt, KoeienPha: Integer; {key-values}
  i: Integer;
  {$ifdef test2}
  S: String;
  {$endif}

Procedure Set_fDn;
  {-Corr. fact. nitraatuitspoeling}
begin
  {$ifdef test}
  Application.MessageBox( 'Set_fDn', 'Info', MB_OKCANCEL );
  {$endif}
  with EP[ cEP0 ].xInDep.Items[ cTbCorrFactNuitsp ] do
    fDn := GetValue( 1, Gt );
  {$ifdef test}
  Application.MessageBox( 'fDn Set', 'Info', MB_OKCANCEL );
  {$endif}
end;

Function SetKeyValues( var IErr: Integer ): Boolean;
  {Assign valid values for Gt, Landgebr, BodemSrt, KoeienPha}
begin
  Result := False;
  with EP[ indx-1 ].xDep do begin {-Value of indx MUST be set by boot-procedure}
    Gt := Trunc( Items[ cTbGt ].EstimateY( x, Direction ) );
    if ( Gt < cGtMin ) or ( Gt > cGtMax ) then begin
      IErr := cInvalidGt; Exit;
    end;
    Landgebr := Trunc( Items[ cTbLandgebr ].EstimateY( x, Direction ) );
    if ( Landgebr < cLandgebrMin ) or ( Landgebr > cLandgebrMax ) then begin
      IErr := cInvalidLandgebr; Exit;
    end;
    BodemSrt := Trunc( Items[ cTbBodemSrt ].EstimateY( x, Direction ) );
    if ( BodemSrt < cBodemSrtMin ) or ( BodemSrt > cBodemSrtMax ) then begin
      IErr := cInvalidBodemSrt; Exit;
    end;
    KoeienPha := Trunc( Items[ cTbKoeienPha ].EstimateY( x, Direction ) );
    if ( KoeienPha < cKoeienPhaMin ) or ( KoeienPha > cKoeienPhaMax ) then begin
      IErr := cInvalidKoeienPha; Exit;
    end;
  end;
  Result := True;
end;

Function SetUnbasis( var IErr: Integer ): Boolean;
var
  Ub: Double; {-Basisuitspoeling (kg/ha/jr), tabel 2 pag. 10}
  Function SetUb( var IErr: Integer ): Boolean;
    {-Constante in form. 1 voor basisuitspoeling pag. 9}
    var
      RelBasisUitsp, RefBasisUitsp: Double;
    Function GetRefBasisUitsp( var IErr: Integer ): Double;
    begin
      Result :=
        EP[ indx-1 ].xDep.Items[ cTbRefBasisNuitsp ].EstimateY( x, Direction );
      if ( Result >= 0 ) then
        IErr := cNoError
      else
        IErr := cRefBasisUitspOngeldig;
    end;
  begin {-Function SetUb}
    IErr := cNoError; Result := true;
    with EP[ cEP0 ].xInDep.Items[ cTbRelBasisUitsp ] do
      RelBasisUitsp := GetValue( LandGebr, BodemSrt );
    if ( RelBasisUitsp < 0 ) then begin
      IErr := Trunc( RelBasisUitsp ); Result := false;
    end else begin
      RefBasisUitsp := GetRefBasisUitsp( IErr );
      if ( IErr = cNoError ) then
        Ub := RelBasisUitsp * RefBasisUitsp
      else
        Result := false;
    end;
  end; {-Function SetUb}
  Function f: Double;
    {-Fractie in form. 1 voor basisuitspoeling pag. 9}
  begin
    with EP[ cEP0 ].xInDep.Items[ cTbHumusgehalte ] do
      Result := GetValue( 1, 4 );
  end;
  Function H: Double;
    {-Humusgehalte form. 1 voor basisuitspoeling pag. 9}
  begin
    with EP[ cEP0 ].xInDep.Items[ cTbHumusgehalte ] do
      Result := GetValue( 1, BodemSrt );
  end;
begin
  Result := SetUb( IErr );
  if Result then
    Unbasis := Ub + f * H;
end;

Function SetUnWinter( var IErr: Integer ): Boolean;
var
  a: Double;
  Function Nwinter: Double;
  begin
    with EP[ indx-1 ].xDep do {-Value of indx MUST be set by boot-procedure}
      Result := Items[ cTbNniveauWNTR ].EstimateY( x, Direction );
  end;
  Function Seta( var IErr: Integer ): Boolean;
    {-Constante in form. 1 voor basisuitspoeling pag. 9}
  begin
    IErr := cNoError; Result := true;
    with EP[ cEP0 ].xInDep.Items[ cTbUitspFractWntr ] do
      a := GetValue( 1, LandGebr );
    if ( a < 0 ) then begin
      IErr := Trunc( a ); Result := false;
    end;
  end;
begin
  Result := Seta( IErr );
  if Result then
    Un_winter := a * Nwinter;
end;

Function Set_Un_niveau( var IErr: Integer ): Boolean;
var
  a, b, cmax,    {-Constants of rel. N-uitsp. (tabel 5, p. 11)}
  Nurine,
  fb,
  Ntot,
  FUn_weide: Double;

  Function Set_a_b_max_Values( var IErr: Integer ): Boolean;
  var
    i: Integer;
  begin
    IErr := cNoError; Result := false;
    i := ( Bodemsrt-1 ) * 3 + 1;
    with EP[ cEP0 ].xInDep.Items[ cTb_a_b_max ] do begin
      a   := GetValue( LandGebr, i );
      if ( a <= 0 ) then begin IErr := Trunc( a ); exit; end;
      b   := GetValue( LandGebr, i+1 );
      if ( b <= 0 ) then begin IErr := Trunc( b ); exit; end;
      cmax := GetValue( LandGebr, i+2 );
      if ( cmax <= 0 ) then begin IErr := Trunc( cmax ); exit; end;
    end;
    Result := true;
  end;

  Procedure SetNniveau;
  begin
    with EP[ indx-1 ].xDep do {-Value of indx MUST be set by boot-procedure}
      Nniveau := Items[ cTbNniveauZMR ].EstimateY( x, Direction );
  end;

  Function FUn_wz( const Nniveau: Double; var IErr: Integer ): Double;
    {-Uitspoelingsfractie (-); form. 4 pag 11. Call 'Set_a_b_max_Values' first!}
  begin
    IErr := cNoError; Result := 1;
    try
      result := cmax / ( ( 1 + exp( - a * ( Nniveau - b ) ) ) * 100 );
    except
      IErr := cErrCalcFUn_wz;
    end;
    {$ifdef test2}
    S := FloatToStrF( a, ffGeneral, 6, 1 ) + ' ' +
       FloatToStrF( b, ffGeneral, 6, 1 ) + ' ' +
       FloatToStrF( cmax, ffGeneral, 6, 1 ) + ' ' +
       FloatToStrF( Nniveau, ffGeneral, 6, 1 ) + ' ' +
       FloatToStrF( result, ffGeneral, 6, 1 );
    Application.MessageBox( PChar( S ), 'a, b, max, Nniveau, result', MB_OKCANCEL );
    {$endif}
  end;

  function Weidegebruik: Boolean;
  begin
    Result := ( LandGebr = cgras ) and ( KoeienPha > 0 );
  end;

  Function SetNurineAndFb( var IErr: Integer ): Boolean;
  var
    dWei, cOpp, eNH3, deltaN, melk, Nkoe: Double;
    Procedure SetWeidePars;
    begin
      with EP[ cEP0 ].xInDep.Items[ cTbWeidPar ] do begin
        dWei   := GetValue( 1, 1 ); {-Duur v.d. weide per. als fract. voll. weideper.}
        cOpp   := GetValue( 1, 2 ); {-Opp.fractie urinebelast. /koe/ha (-)}
        eNH3   := GetValue( 1, 3 ); {-emissie amoniak (-)}
        deltaN := GetValue( 1, 4 ); {-denitrificatie (-)}
        melk   := GetValue( 1, 5 ); {-melkgift (kg/jr)}
      end;
    end;
  begin
    IErr := cNoError; Result := true;
    Try
      {$ifdef test}
      Application.MessageBox( 'SetWeidePars', 'Info', MB_OKCANCEL );
      {$endif}
      SetWeidePars;
      {$ifdef test}
      Application.MessageBox( 'CalcNkoe', 'Info', MB_OKCANCEL );
      {$endif}
      Nkoe := 30 + 2.5*( melk - 5000 )*Power( 0.85, (400 - Nniveau)/100 ) / 1000;
      Nkoe := Max( Nkoe, 0 );
      {$ifdef test}
      Application.MessageBox( 'fb', 'Info', MB_OKCANCEL );
      {$endif}
      fb := 1 - IntPower( ( 1 - cOpp ), KoeienPha );
      {$ifdef test}
      Application.MessageBox( 'NUrine', 'Info', MB_OKCANCEL );
      {$endif}
      NUrine := dWei * KoeienPha * Nkoe * ( 1 - eNH3 ) * ( 1 - deltaN ) / fb;
      NUrine := Max( NUrine, 0 );
    except
      IErr := cErrorCalcNurine; Result := false;
    end;
  end; {-SetNurineAndFb}
begin
  Result := Set_a_b_max_Values( IErr );
  if ( not Result ) then exit;

  SetNniveau;
  Un_niveau := Nniveau * FUn_wz( Nniveau, IErr );
  {-Zonder beweiding is Un_niveau nu berekend}

  {$ifdef test2}
  S := FloatToStrF( Nniveau, ffGeneral, 6, 1 ) + ' ' +
       FloatToStrF( Un_niveau, ffGeneral, 6, 1 );
  Application.MessageBox( PChar( S ), 'Nniveau, Un_niveau', MB_OKCANCEL );
  {$endif}

  if ( IErr <> cNoError ) then begin
    Result := false; exit;
  end;

  if weidegebruik then begin
    Result := SetNurineAndFb( IErr );
    if Result then begin
      Ntot      := Nniveau + Nurine;
      FUn_weide := FUn_wz( Ntot, IErr );
      if ( IErr <> cNoError ) then begin
        Result := false; exit;
      end else begin
        Un_niveau := Un_niveau * ( 1 - fb ) + FUn_weide * Ntot * fb;
      end;
    end; {-if result}
  end; {-if weidegebruik}
end; {-Set_Un_niveau}

Function NatGWaanv: Double;
begin
  with EP[ indx-1 ].xDep do {-Value of indx MUST be set by boot-procedure}
    Result := Items[ cTbNatGWaanv ].EstimateY( x, Direction );
end;

begin
  IErr := cUnknownError;
  for i := 1 to cNrOfDepVar do {-Default speed = 0}
    dydx[ i ] := 0;

  {-Geef de aanroepende procedure een handvat naar het ModelProfiel}
  if ( nDC > 0 ) then
    aModelProfile := @ModelProfile
  else
    aModelProfile := NIL;
  
  if ( Context = UpdateYstart ) then begin
    {-Override initial values on ystart-vector here}

    {-Converteer dag-waarden uit tijdreeksen en invoertijdstippen afkomstig van
        de Shell naar jaren}
    if ( indx = cBoot2 ) then
      ScaleTimesFromShell( cFromDayToYear, EP );

    IErr := cNoError;

  end else begin             {-Fill dydx-vector}
    {-Set Key-values}
    if not SetKeyValues( IErr ) then
      exit; {-Gt, Landgebr, BodemSrt, KoeienPha}
    dydx[ cNatGWaanv ] := NatGWaanv;
    Set_fDn; {-Corr. fact. nitraatuitspoeling}
    if ( fDn = 0 ) then begin {-Doe geen moeite als er door denitrificatie toch}
      dydx[ cNuitsp ] := 0;   {-geen uitspoeling is}
      IErr := cNoError;
      exit;
    end;
    if SetUnbasis( IErr ) and
       SetUnWinter( IErr ) and
       Set_Un_niveau( IErr ) then begin
       dydx[ cNuitsp ] := fDn *           {-effect denitrificatie}
                                ( Unbasis +     {-basisuitspoeling}
                                  Un_winter +   {-uitsp. buiten het groeiseizoen}
                                 Un_niveau );  {-uitsp. i.h. groeiseizoen}
       dydx[ cNGewasOpn ] := Nniveau - Un_niveau;
      {$ifdef test2}
      S := FloatToStrF( NatGWaanv, ffGeneral, 6, 1 ) + ' ' +
           IntToStr( Gt ) + ' ' +
           IntToStr( LandGebr ) + ' ' +
           IntToStr( BodemSrt ) + ' ' +
           IntToStr( KoeienPha );
      Application.MessageBox( PChar( S ), 'Pr, Gt, LandGebr, BodemSrt, KoeienPha',
       MB_OKCANCEL );
      S := FloatToStrF( fDn, ffGeneral, 6, 1 ) + ' ' +
           FloatToStrF( Unbasis, ffGeneral, 6, 1 ) + ' ' +
           FloatToStrF( Un_winter, ffGeneral, 6, 1 ) + ' ' +
           FloatToStrF( Nniveau, ffGeneral, 6, 1 ) + ' ' +
           FloatToStrF( Un_niveau, ffGeneral, 6, 1 );
      Application.MessageBox( PChar( S ), 'fDn, Unbasis, Un_winter, Nniveau, Un_niveau',
       MB_OKCANCEL );
      {$endif}
    end;
  end;
end; {-DerivsProc}

Function DefaultBootEP( const EpDir: String; const BootEpArrayOption: TBootEpArrayOption; var EP: TExtParArray ): Integer;
  {-xDep-tables (Gt, Landgebruik, bodemsoort, KoeienPha, NniveauZMR,
    NniveauWNTR) are NOT set by this boot-procedure: they have to be initialised
    in another way}
Procedure SetMinMaxKeyValues;
  {Assign min/max values for Gt, Landgebr and BodemSrt}
begin
  with EP[ cEP0 ].xInDep.Items[ cTbMinMaxValKeys ] do begin
    cGtMin        := Trunc( GetValue( 1, 1 ) );
    cGtMax        := Trunc( GetValue( 1, 2 ) );
    cLandgebrMin  := Trunc( GetValue( 1, 3 ) );
    cLandgebrMax  := Trunc( GetValue( 1, 4 ) );
    cBodemSrtMin  := Trunc( GetValue( 1, 5 ) );
    cBodemSrtMax  := Trunc( GetValue( 1, 6 ) );
    cKoeienPhaMin := Trunc( GetValue( 1, 7 ) );
    cKoeienPhaMax := Trunc( GetValue( 1, 8 ) );
  end;
end;
Begin
  Result := DefaultBootEPFromTextFile( EpDir, BootEpArrayOption, cModelID, cNrOfDepVar, nDC, cNrXIndepTblsInEP0,
                                       cNrXdepTblsInEP0, Indx, EP );
  if ( Result = cNoError ) then
    SetMinMaxKeyValues;
end;

Function TestBootEP( const EpDir: String; const BootEpArrayOption: TBootEpArrayOption; var EP: TExtParArray ): Integer;
  {-Apart from the defaults for TestBootEP, this procedure also sets the
    xDep-tables (Gt, Landgebruik, bodemsoort, KoeienPha, NniveauZMR,
    NniveauWNTR), so the model is ready-to-run }
Begin
  Result := DefaultBootEP( EpDir, BootEpArrayOption, EP );
  if ( Result <> cNoError ) then exit;
  Result := DefaultTestBootEPFromTextFile( EpDir, BootEpArrayOption, cModelID, cnRP + cnSQ + cnRQ, Indx,
                                           EP );
  if ( Result <> cNoError ) then exit;
  SetReadyToRun( EP);
end;

Function BootEPForShell( const EpDir: String; const BootEpArrayOption: TBootEpArrayOption; var EP: TExtParArray ): Integer;
  {-xDep-tables (Gt, Landgebruik, bodemsoort, KoeienPha, NniveauZMR,
    NniveauWNTR) are NOT set by this boot-procedure: they must be supplied
    by the shell }
begin
  Result := DefaultBootEP( EpDir, cBootEPFromTextFile, EP );
  if ( Result = cNoError ) then
    Result := DefaultBootEPForShell( cnRP, cnSQ, cnRQ, Indx, EP );
end;

Exports DerivsProc       index cModelIndxForTDSmodels, {999}
        DefaultBootEP    index cBoot0, {1}
        TestBootEP       index cBoot1, {2}
        BootEPForShell   index cBoot2; {3}
begin
  {-This 'DLL-Main-block' is executed  when the DLL is initially loaded into
    memory (Reason = DLL_PROCESS_ATTACH)}
  DLLProc := @MyDllProc;
  Indx := cBootEPArrayVariantIndexUnknown;
  if ( nDC > 0 ) then
    ModelProfile := TModelProfile.Create( nDC );
end.
