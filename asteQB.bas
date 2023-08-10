'-----------------------------------------------------------------
'-----                                                       -----
'-----                A S T E R O I D E S                    -----
'----                                                        -----
'----             Programado por: Juan Eguia                 -----
'----                                                        -----
'-----------------------------------------------------------------
'----                C O N S T A N T E S                     -----
'-----------------------------------------------------------------
Const azulado = _RGB32(0, 0, 25)
Const blanco = _RGB32(211, 211, 211)
Const gris = _RGB32(105, 122, 122)
Const grisaste = _RGB32(150, 150, 150)
Const negro = _RGB32(0, 0, 0)
Const grisaceo = _RGB32(128, 133, 144)
Const amarillo = _RGB32(194, 128, 0)
Const amarillo_disparo = _RGB32(255, 255, 89)
Const verde = _RGB32(0, 205, 28)
Const azulc = _RGB32(6, 205, 222)
Const rojo = _RGB32(255, 55, 0)
Const mconfg = _RGB32(0, 100, 100)

Const RES_X = 1000
Const RES_Y = 700
Const FPS = 60

Const NRO_ESTRELLAS_FIJAS = 1500
Const NRO_ESTRELLAS_3D = 400

Const NRO_MAX_DISPAROS_JUGADOR = 7
Const CADENCIA_DISPARO = 9

Const NRO_PARTICULAS_ESPARCIDAS = 40
Const NRO_EXPLOSIONES_SIMULTANEAS = 9
Const NRO_MAX_ASTEROIDES = 399

Const SUMA_PTOS_ASTE = 100
Const AxPTOS_EXTRA = 5000
Const TIME_UP = 32000

Const pi = 3.14159
Const coemp = -57.2957795131

'-----------------------------------------------------------------
'----             V A R I A B L E S  (OBJETOS)
'-----------------------------------------------------------------
Type nave
    pic As Long
    rotado As Long
    x As Single
    y As Single
    radio As Integer
    ancho As Integer
    alto As Integer
    velX As Single
    velY As Single
    vel As Single
    inerc As Single
    grados As Single
    viejosGrados As Single
    velGiro As Single
    aceleracion As Single
    deceleracion As Single
    velMax As Single
    avanza As Integer
    gira As Integer
    explosion As Integer
    duraExplosion As Integer
    invisible As Integer
End Type

Type aste
    rotado As Long
    x As Single
    y As Single
    radio As Integer
    velX As Single
    velY As Single
    vel As Single
    grados As Integer
    activo As Integer
End Type

Type disparo
    x As Integer
    y As Integer
    radio As Integer
    velX As Single
    velY As Single
    vel As Single
    activo As Integer
End Type

Type explosion
    x As Single
    y As Single
    velX As Single
    velY As Single
    ancho As Integer
    duracion As Integer
    nave As Integer
End Type

Type estrella
    x As Integer
    y As Integer
    ancho As Integer
    alto As Integer
    brillo As Integer
End Type

Type estr3d
    x As Single
    y As Single
    velX As Single
    velY As Single
    ancho As Integer
    alto As Integer
    brillo As Integer
End Type

Type titulo
    presentacion As Long
    sizeX As Single
    sizeY As Single
End Type

Type ptomira
    x As Integer
    y As Integer
End Type

'-----------------------------------------------------------------
'----            D I M  (RESERVAR ESPACIO EN MEMORIA)
'-----------------------------------------------------------------
Dim nave As nave
Dim aste(NRO_MAX_ASTEROIDES) As aste
Dim disparo(NRO_MAX_DISPAROS_JUGADOR) As disparo
Dim explosion(NRO_EXPLOSIONES_SIMULTANEAS, NRO_PARTICULAS_ESPARCIDAS) As explosion
Dim estrella(NRO_ESTRELLAS_FIJAS) As estrella
Dim estr3d(NRO_ESTRELLAS_3D) As estr3d
Dim titulo As titulo
Dim ptomira As ptomira

Dim a As Integer
Dim b As Integer
Dim c As Integer
Dim fotogramas As Integer
Dim comenzar As _Byte
Dim cadencia As Integer
Dim ciclos As Integer
Dim puntos As Long
Dim nivel As Integer
Dim vidas As Integer
Dim asteroidesAbatidos As Integer
Dim powDisparo As Integer
Dim record(10) As Long
Dim rnivel(10) As Integer
Dim nombre(10) As String
Dim nivel_superado As _Bit
Dim exit_Esc As _Bit
Dim game_over As _Bit

'----------------------- GRAFICOS ---------------------
Dim astePic As Long
Dim losniveles As Long
Dim gameOver As Long
Dim nivelSuperado As Long
Dim lavidaextra As Long
Dim elranking As Long
Dim enhorabuena As Long
Dim fastshoot As Long

'----------------------- SONIDOS ----------------------
Dim sonido_laser As Long
Dim sonido_caer As Long
Dim sonido_go As Long
Dim sonido_gameover As Long
Dim sonido_explosion As Long
Dim sonido_navexplota As Long
Dim sonido_superado As Long
Dim sonido_vibracion As Long
Dim sonido_impactomini As Long
Dim sonido_levelup As Long
Dim sonido_extralive As Long

'---------------------------------------------------------------
'------------------ INICIALIZACION GENERAL ---------------------
'---------------------------------------------------------------
Screen _NewImage(RES_X, RES_Y, 32)
_Title " AsteQB by Juan Eguia "
_ScreenMove _DesktopWidth / 2 - _Width / 2, _DesktopHeight / 2 - _Height / 2

_PrintMode _KeepBackground
'_FullScreen
'_MouseShow
Randomize Timer

Cls , azulado
Color amarillo, azulado
Locate 19, 52
Print " Cargando... "

updatesGraficos
updatesSonido
updatesGenerales

'-------------------------------------------------------------
'                   P R E S E N T A C I O N
'-------------------------------------------------------------
'_SndPlayCopy sonido_caer
Cls , azulado
ciclos = 0

Do
    _Limit fotogramas
    PCopy _Display, 1

    el_fondoEstrellas
    las_estrellas3D

    If _KeyDown(13) Then comenzar = -1

    pantallaPresentacion titulo, ciclos
    mostrar_records record(), rnivel(), nombre()

    _Display
    PCopy 1, _Display

Loop Until comenzar

soniquete 100, 600

'-------------------------------------------------------------
'---------    I N I C I O   D E   N I V E L   x      ---------
'-------------------------------------------------------------
Do
    Do
        updatesNivelX

        For a = 1 To nivel * 3
            creaAsteroides 100, 0, 0 'radio Inicial (Grandes)
        Next a

        '-------------------------------------------------------------
        '
        '                B U C L E   P R I N C I P A L
        '
        '-------------------------------------------------------------
        Do
            _Limit fotogramas
            PCopy _Display, 1

            el_fondoEstrellas
            las_estrellas3D

            If _KeyDown(100306) Then Input fotogramas
            If _KeyDown(8) Then Sleep 99
            If _KeyDown(27) Then exit_Esc = -1

            While _MouseInput
                ptomira.x = _MouseX
                ptomira.y = _MouseY
            Wend

            la_nave
            el_disparo
            los_asteroides
            la_explosion

            mostrarMarcadores

            If asteroidesAbatidos >= (nivel * 3) + (nivel * 3 * 2) + (nivel * 3 * 2 * 2) Then
                nivel_superado = -1
            End If

            ciclos = ciclos + 1

            If cadencia > 0 Then cadencia = cadencia - 1
            If ciclos = 32000 Then ciclos = 1

            _Display
            PCopy 1, _Display

        Loop Until vidas < 0 Or nivel_superado Or exit_Esc

        '-----------------------------------------------------------
        '                   G A M E   O V E R
        '-----------------------------------------------------------
        _SndPlayCopy sonidogameover

        Do
            _Limit 60
            PCopy _Display, 1

            If _KeyDown(27) Then exit_Esc = -1

            While _MouseInput
                ptomira.x = _MouseX
                ptomira.y = _MouseY
            Wend

            ciclos = ciclos + 1

            If cadencia > 0 Then cadencia = cadencia - 1
            If ciclos = 32000 Then ciclos = 0

            _Display
            PCopy 1, _Display

        Loop Until exit_Esc Or nivel_superado

    Loop Until exit_Esc

Loop Until exit_Esc

'-----------------------------------------------------------
'                   F I N   P R O G R A M A
'-----------------------------------------------------------
'salir
Beep
System

'===========================================================
'
'                   S U B R U T I N A S
'
'-----------------------------------------------------------
Sub la_nave

    Dim a As Integer
    Shared nave As nave

    '-------------------------------------------
    sub_leerTeclado nave
    sub_actualizaAngulo nave
    sub_actualizaPos nave
    sub_checkLimites nave
    sub_larotacion nave
    sub_Invisible nave

    If nave.explosion <= 0 Then
        '_PutImage (nave.x + nave.ancho / 2, nave.y + nave.alto / 2), nave.pic
        _PutImage (nave.x - _Width(nave.rotado) / 2, nave.y - _Height(nave.rotado) / 2), nave.rotado
    Else
        nave.explosion = nave.explosion - 1
        If nave.explosion = 0 Then nave.invisible = 200
    End If

End Sub

'------------------------------------------------------------
Sub sub_leerTeclado (nave As nave)

    Dim tecla As Long

    tecla = _KeyHit

    If tecla = 18432 Then nave.avanza = 1
    If tecla = -18432 Then nave.avanza = 0
    'If tecla = 20480 Then nave.avanza = 0
    'If tecla = -20480 Then nave.avanza = 0
    If tecla = 19200 Then nave.gira = 1
    If tecla = -19200 Then nave.gira = 0
    If tecla = 19712 Then nave.gira = -1
    If tecla = -19712 Then nave.gira = 0

    'If tecla = 32 Or tecla = 88 Or tecla = 120 Then iniciaDisparo nave
    If _KeyDown(32) Or _KeyDown(88) Or _KeyDown(120) Then iniciaDisparo nave

End Sub

'------------------------------------------------------------
Sub sub_actualizaAngulo (nave As nave)

    nave.grados = nave.grados + nave.gira * nave.velGiro
    normalizaAngulo nave

    nave.velX = -Sin(nave.viejosGrados)
    nave.velY = -Cos(nave.viejosGrados)

    If nave.gira <> 0 Then nave.inerc = 0

End Sub

'------------------------------------------------------------
Sub sub_actualizaPos (nave As nave)

    If nave.avanza <> 0 Then

        nave.viejosGrados = nave.grados

        If nave.inerc = 0 Then nave.vel = 1

        nave.vel = nave.vel + nave.aceleracion
        nave.inerc = nave.inerc + 1

        sub_estelaPropulsion nave

    ElseIf nave.avanza = 0 Then
        nave.vel = nave.vel - nave.deceleracion
    End If

    If nave.vel < 1 Then nave.vel = 1
    If nave.vel >= nave.velMax Then nave.vel = nave.velMax

    nave.x = nave.x + nave.velX * nave.vel
    nave.y = nave.y + nave.velY * nave.vel

End Sub

'------------------------------------------------------------
Sub sub_Invisible (nave As nave)

    Dim c As Integer
    Dim x As Integer
    Dim y As Integer

    If nave.invisible > 0 Then
        c = Int(Rnd * 225) + 30
        x = nave.x
        y = nave.y

        Circle (x, y), nave.radio * 2, _RGB32(60, c, 255)
        Paint (x, y), _RGB32(60, c, 255), _RGB32(60, c, 255)
        nave.invisible = nave.invisible - 1
    End If

End Sub

'------------------------------------------------------------
Sub sub_estelaPropulsion (nave As nave)

    Dim c As Integer
    Dim r_g As Single

    r_g = nave.grados * (180 / _Pi) + 180
    c = Int(Rnd * 175) + 25
    PSet (nave.x, nave.y), _RGB32(255, c, 0)
    Draw "TA" + LTrim$(Str$(r_g)) + "U10U" + LTrim$(Str$(Int(Rnd * 35)))

    For a = 1 To 11
        PSet (nave.x + Int(Rnd * 9) - 4, nave.y + Int(Rnd * 9) - 4), _RGB32(255, c, 0)
        Draw "TA" + LTrim$(Str$(r_g)) + "U15U" + LTrim$(Str$(Int(Rnd * 35)))
    Next a

End Sub

'------------------------------------------------------------
Sub sub_checkLimites (nave As nave)

    If nave.x - nave.radio > RES_X Then nave.x = 0
    If nave.x + nave.radio < 0 Then nave.x = RES_X
    If nave.y - nave.radio > RES_Y Then nave.y = 0
    If nave.y + nave.radio < 0 Then nave.y = RES_Y

End Sub

'------------------------------------------------------------
Sub normalizaAngulo (nave As nave)

    If nave.grados < 0 Then
        nave.grados = nave.grados + 2 * _Pi
        Exit Sub
    End If

    If nave.grados > 2 * _Pi Then
        nave.grados = nave.grados - 2 * _Pi
        Exit Sub
    End If

End Sub

'------------------------------------------------------------
Sub sub_larotacion (na As nave)

    Dim Polar As Integer
    Dim px(3) As Integer
    Dim py(3) As Integer
    Dim Xoffset As Integer
    Dim Yoffset As Integer
    Dim SINr As Single
    Dim COSr As Single
    Dim Left As Integer
    Dim Top As Integer
    Dim Right As Integer
    Dim Bottom As Integer
    Dim x As Single
    Dim y As Single
    Dim RotWidth As Integer
    Dim RotHeight As Integer

    '---------------------------------------
    If na.rotado Then _FreeImage na.rotado

    px(0) = -_Width(na.pic) / 2
    py(0) = -_Height(na.pic) / 2
    px(1) = px(0)
    py(1) = _Height(na.pic) / 2
    px(2) = _Width(na.pic) / 2
    py(2) = py(1)
    px(3) = px(2)
    py(3) = py(0)
    SINr = Sin(na.grados * (180 / _Pi) / 57.2957795131)
    COSr = Cos(-na.grados * (180 / _Pi) / 57.2957795131)
    Left = 0
    Top = 0
    Right = 0
    Bottom = 0

    Polar = 0
    Do
        x = (px(Polar) * COSr + SINr * py(Polar))
        y = (py(Polar) * COSr - px(Polar) * SINr)
        px(Polar) = x
        py(Polar) = y
        If px(Polar) < Left Then Left = px(Polar)
        If px(Polar) > Right Then Right = px(Polar)
        If py(Polar) < Top Then Top = py(Polar)
        If py(Polar) > Bottom Then Bottom = py(Polar)
        Polar = Polar + 1
    Loop Until Polar = 4

    RotWidth = Right - Left + 1
    RotHeight = Bottom - Top + 1
    Xoffset = RotWidth \ 2
    Yoffset = RotHeight \ 2

    px(0) = px(0) + Xoffset
    px(1) = px(1) + Xoffset
    px(2) = px(2) + Xoffset
    px(3) = px(3) + Xoffset
    py(0) = py(0) + Yoffset
    py(1) = py(1) + Yoffset
    py(2) = py(2) + Yoffset
    py(3) = py(3) + Yoffset

    na.rotado = _NewImage(RotWidth, RotHeight, 32)

    _MAPTRIANGLE (0, 0)-(0, _HEIGHT(na.pic) - 1)-(_WIDTH(na.pic) - 1, _HEIGHT(na.pic) - 1), na.pic TO _
                 (px(0), py(0))-(px(1), py(1))-(px(2), py(2)), na.rotado
    _MAPTRIANGLE (0, 0)-(_WIDTH(na.pic) - 1, 0)-(_WIDTH(na.pic) - 1, _HEIGHT(na.pic) - 1), na.pic TO _
                 (px(0), py(0))-(px(3), py(3))-(px(2), py(2)), na.rotado

End Sub

'-----------------------------------------------------------
Sub iniciaDisparo (nave As nave)

    Dim b As Integer

    Shared disparo() As disparo

    Shared sonido_laser As Long
    Shared powDisparo As Integer
    Shared cadencia As Integer

    If cadencia > 0 Then Exit Sub

    '------------------------------------
    b = 1
    Do
        If disparo(b).activo = 0 Then
            disparo(b).activo = 1
            disparo(b).x = Int(nave.x)
            disparo(b).y = Int(nave.y)
            disparo(b).velX = -Sin(nave.grados)
            disparo(b).velY = -Cos(nave.grados)

            'soniquete 1200, 1800
            _SndPlayCopy sonido_laser
            cadencia = CADENCIA_DISPARO
            Exit Sub
        End If

        b = b + 1
    Loop Until b = powDisparo + 1


End Sub

'-----------------------------------------------------------
Sub el_disparo

    Dim b As Integer

    Shared disparo() As disparo

    For b = 1 To NRO_MAX_DISPAROS_JUGADOR
        If disparo(b).activo > 0 Then
            disparo(b).x = Int(disparo(b).x + disparo(b).velX * disparo(b).vel)
            disparo(b).y = Int(disparo(b).y + disparo(b).velY * disparo(b).vel)

            Circle (disparo(b).x, disparo(b).y), disparo(b).radio, amarillo_disparo
            Paint (disparo(b).x, disparo(b).y), amarillo_disparo, amarillo_disparo

            If disparo(b).x - disparo(b).radio > RES_X Or disparo(b).x < -disparo(b).radio Then disparo(b).activo = 0
            If disparo(b).y - disparo(b).radio > RES_Y Or disparo(b).y < -disparo(b).radio Then disparo(b).activo = 0
        End If
    Next b

End Sub

'-----------------------------------------------------------
Function check_colisionesVsDisparo (a As Integer, b As Integer, aste() As aste, disparo() As disparo)

    Dim centroAsteX As Single
    Dim centroAsteY As Single
    Dim cateto1 As Integer
    Dim cateto2 As Integer
    Dim hipo2 As Long

    centroAsteX = aste(a).x + aste(a).radio
    centroAsteY = aste(a).y + aste(a).radio

    check_colisionesVsDisparo = 0
    cateto1 = Int(Abs(centroAsteX - disparo(b).x))
    cateto2 = Int(Abs(centroAsteY - disparo(b).y))
    hipo2 = cateto1 * cateto1 + cateto2 * cateto2

    If hipo2 <= (aste(a).radio + disparo(b).radio) * (aste(a).radio + disparo(b).radio) Then
        check_colisionesVsDisparo = -1
        disparo(b).activo = 0
    End If

End Function

'-----------------------------------------------------------
Function check_colisionesVsNave (a As Integer, b As Integer, aste() As aste, nave As nave)

    Dim centroAsteX As Single
    Dim centroAsteY As Single
    Dim cateto1 As Integer
    Dim cateto2 As Integer
    Dim hipo2 As Long

    '------------------------------------------
    centroAsteX = aste(a).x + aste(a).radio
    centroAsteY = aste(a).y + aste(a).radio

    check_colisionesVsNave = 0
    cateto1 = Int(Abs(centroAsteX - nave.x))
    cateto2 = Int(Abs(centroAsteY - nave.y))
    hipo2 = cateto1 * cateto1 + cateto2 * cateto2

    If hipo2 <= (aste(a).radio + nave.radio) * (aste(a).radio + nave.radio) Then
        check_colisionesVsNave = -1
    End If

End Function

'-----------------------------------------------------------
Sub creaAsteroides (radio As Integer, x As Single, y As Single)

    Dim a As Integer

    Shared aste() As aste
    Shared nivel As Integer

    a = 1
    Do
        If aste(a).activo = 0 Then
            If radio = 100 Then
                aste(a).x = Int(Rnd * RES_X)
                aste(a).y = Int(Rnd * RES_Y)
                aste(a).vel = 1 + Int(Rnd * 4) / 10 + nivel / 12
            Else
                aste(a).x = x
                aste(a).y = y
                aste(a).vel = 1 + Int(Rnd * 4) / 10 + nivel / 6
            End If

            aste(a).grados = Int(Rnd * 360) * (_Pi / 180)
            aste(a).velX = Sin(aste(a).grados)
            aste(a).velY = -Cos(aste(a).grados)

            If radio = 100 Then
                aste(a).radio = 50 + Int(Rnd * 15)
            ElseIf radio >= 50 Then
                aste(a).radio = 25 + Int(Rnd * 9)
            ElseIf radio >= 25 Then
                aste(a).radio = 10 + Int(Rnd * 9)
            Else
                Exit Sub
            End If

            aste(a).activo = 1
            Exit Sub
        End If

        a = a + 1
    Loop Until a >= NRO_MAX_ASTEROIDES

End Sub

'-----------------------------------------------------------
Sub los_asteroides

    Dim a As Integer
    Dim b As Integer
    Dim x As Integer
    Dim y As Integer
    Dim r As Integer

    Shared aste() As aste
    Shared disparo() As disparo
    Shared nave As nave

    Shared astePic As Long
    Shared asteroidesAbatidos As Integer
    Shared vidas As Integer
    Shared puntos As Long

    '-------------------------------------
    For a = 1 To NRO_MAX_ASTEROIDES

        If aste(a).activo > 0 Then
            x = aste(a).x
            y = aste(a).y
            r = aste(a).radio

            _PutImage (x, y)-Step(r * 2, r * 2), astePic

            aste(a).x = aste(a).x + aste(a).velX * aste(a).vel
            aste(a).y = aste(a).y + aste(a).velY * aste(a).vel
            check_limitesAste aste(), a

            '---------------- Check-COLISIONES vs Nave -----------------------
            If nave.explosion > 0 Or nave.invisible > 0 Then
                'No colision porque estamos Invisibles o Estallando
            Else
                If check_colisionesVsNave(a, b, aste(), nave) Then
                    inicia_explosion aste(a).x, aste(a).y, 0
                    creaAsteroides aste(a).radio, aste(a).x, aste(a).y
                    creaAsteroides aste(a).radio, aste(a).x, aste(a).y
                    aste(a).activo = 0
                    asteroidesAbatidos = asteroidesAbatidos + 1
                    nave.explosion = nave.duraExplosion
                    inicia_explosion nave.x, nave.y, 1
                    vidas = vidas - 1
                End If
            End If

            '---------------- Check-COLISIONES vs Disparos -------------------
            For b = 1 To NRO_MAX_DISPAROS_JUGADOR
                If disparo(b).activo > 0 Then
                    If check_colisionesVsDisparo(a, b, aste(), disparo()) Then
                        inicia_explosion aste(a).x, aste(a).y, 0
                        creaAsteroides aste(a).radio, aste(a).x, aste(a).y
                        creaAsteroides aste(a).radio, aste(a).x, aste(a).y
                        aste(a).activo = 0
                        asteroidesAbatidos = asteroidesAbatidos + 1
                        puntos = puntos + 100 + Int(Rnd * 5) * 10
                    End If
                End If
            Next b
        End If

    Next a

End Sub

'-----------------------------------------------------------
Sub check_limitesAste (aste() As aste, a As Integer)

    If aste(a).x - aste(a).radio * 2 > RES_X Then aste(a).x = -aste(a).radio * 2
    If aste(a).x + aste(a).radio * 2 < 0 Then aste(a).x = RES_X
    If aste(a).y - aste(a).radio * 2 > RES_Y Then aste(a).y = -aste(a).radio * 2
    If aste(a).y + aste(a).radio * 2 < 0 Then aste(a).y = RES_Y

End Sub

'-----------------------------------------------------------
Sub inicia_explosion (x As Single, y As Single, nave As Integer)

    Dim a As Integer
    Dim i As Integer

    Shared explosion() As explosion
    Shared sonido_explosion As Long

    _SndPlay sonido_explosion

    i = 1
    Do
        If explosion(i, 1).duracion <= 0 Then
            For a = 1 To NRO_PARTICULAS_ESPARCIDAS
                explosion(i, a).x = x
                explosion(i, a).y = y

                If nave = 0 Then
                    explosion(i, a).velX = (Int(Rnd * 100) - 50) / 10
                    explosion(i, a).velY = (Int(Rnd * 100) - 50) / 10
                Else
                    explosion(i, a).velX = (Int(Rnd * 100) - 50) / 5
                    explosion(i, a).velY = (Int(Rnd * 100) - 50) / 5
                End If

                explosion(i, a).ancho = Int(Rnd * 5)
                explosion(i, a).duracion = 150
                explosion(i, a).nave = nave
            Next a
            Exit Sub
        End If

        i = i + 1
    Loop Until i >= 9
End Sub

'-----------------------------------------------------------
Sub la_explosion

    Dim a As Integer
    Dim i As Integer

    Dim x As Integer
    Dim y As Integer
    Dim ancho As Integer

    Shared explosion() As explosion

    For i = 1 To NRO_EXPLOSIONES_SIMULTANEAS
        If explosion(i, 1).duracion > 0 Then

            explosion(i, 1).duracion = explosion(i, 1).duracion - 1

            For a = 1 To NRO_PARTICULAS_ESPARCIDAS
                x = explosion(i, a).x
                y = explosion(i, a).y
                ancho = explosion(i, a).ancho

                If explosion(i, a).nave = 0 Then
                    Line (x, y)-Step(ancho, ancho), grisaste, BF
                Else
                    Line (x, y)-Step(ancho, ancho), _RGB32(255, Int(Rnd * 200) + 55, 0), BF
                End If

                explosion(i, a).x = explosion(i, a).x + explosion(i, a).velX
                explosion(i, a).y = explosion(i, a).y + explosion(i, a).velY
            Next a

        End If
    Next i

End Sub

'-----------------------------------------------------------
Sub el_fondoEstrellas

    Dim a As Integer
    Dim x As Integer
    Dim y As Integer
    Dim ancho As Integer
    Dim brillo As Integer

    Shared estrella() As estrella

    For a = 1 To NRO_ESTRELLAS_FIJAS
        x = estrella(a).x
        y = estrella(a).y
        ancho = estrella(a).ancho
        brillo = estrella(a).brillo
        Line (x, y)-Step(ancho, ancho), _RGB32(brillo, brillo, brillo), BF
    Next a

End Sub

'-----------------------------------------------------------
Sub las_estrellas3D

    Dim a As Integer
    Dim x As Integer
    Dim y As Integer
    Dim ancho As Integer
    Dim brillo As Integer
    Dim distancia As Single

    Shared estr3d() As estr3d

    For a = 1 To NRO_ESTRELLAS_3D
        x = estr3d(a).x
        y = estr3d(a).y
        ancho = estr3d(a).ancho

        brillo = estr3d(a).brillo
        distancia = Sqr(Abs((x - RES_X / 2) * (x - RES_X / 2)) + Abs((y - RES_Y / 2) * (y - RES_Y / 2)))

        ancho = ancho + Int(distancia / 250)
        brillo = brillo + Int(distancia / 4)

        If ancho > 3 Then ancho = 3
        If brillo > 255 Then brillo = 255

        Line (x, y)-Step(ancho, ancho), _RGB32(brillo, brillo, brillo), BF

        estr3d(a).x = estr3d(a).x + estr3d(a).velX
        estr3d(a).y = estr3d(a).y + estr3d(a).velY

        check_limitesEstrellas3D estr3d(), a
    Next a

End Sub

'-----------------------------------------------------------
Sub check_limitesEstrellas3D (estr3d() As estr3d, a As Integer)

    If estr3d(a).x > RES_X Or estr3d(a).x < 0 Or estr3d(a).y > RES_Y Or estr3d(a).y < 0 Then
        instancia_Estrellas3D estr3d(), a
    End If

End Sub

'-----------------------------------------------------------
Sub soniquete (uno As Integer, dos As Integer)

    Dim a As Integer

    For a = uno To dos Step 50
        Sound a, 0.2
    Next a

End Sub

'-----------------------------------------------------------
Sub mostrarMarcadores

    Shared nave As nave

    Shared puntos As Long
    Shared nivel As Integer
    Shared vidas As Integer
    Shared nombre() As String
    Shared record() As Long

    '--------------------------------------
    _PutImage (610, 0)-Step(30, 20), nave.pic

    Color amarillo, azulado
    Locate 1, 1
    Print " Puntos:";

    Color verde, azulado
    Print Using "######"; puntos

    Color amarillo, azulado
    Locate 1, 20
    Print " Record:";

    Color verde, azulado
    Print Using "######"; record(1);

    Color azulc, azulado
    Print " [ "; nombre(1); " ] "

    Color amarillo, azulado
    Locate 1, 60
    Print " Nivel:";

    Color verde, azulado
    Print Using "##"; nivel

    If vidas < 0 Then
        Locate 1, 82
        Print "0"
    Else
        Locate 1, 82
        Print LTrim$(Str$(vidas))
    End If

End Sub

'-----------------------------------------------------------
Sub mostrar_records (record() As Long, rnivel() As Integer, nombre() As String)

    Dim a As Integer

    For a = 1 To 9
        Color amarillo, azulado
        Locate a + 24, 38
        Print LTrim$(Str$(a)); ". "

        Color verde, azulado
        Locate a + 24, 42
        Print nombre(a)

        Color amarillo, azulado
        Locate a + 24, 72
        Print Using "##"; rnivel(a)

        Locate a + 24, 80
        Print Using "######"; record(a)
    Next a

End Sub

'-----------------------------------------------------------
Sub pantallaPresentacion (titulo As titulo, ciclos As Integer)

    Dim x As Single
    Dim y As Single

    '----------------------------------------
    x = titulo.sizeX
    y = titulo.sizeY

    _PutImage (RES_X / 2 - x / 2, RES_Y / 4 - y / 2)-Step(x, y), titulo.presentacion

    If titulo.sizeX < 900 Then
        titulo.sizeX = titulo.sizeX + 5
        titulo.sizeY = titulo.sizeY + 2
    End If

    If ciclos < 20 Then
        Locate 22, 47
        Print " Pulse ENTER para comenzar... "
    End If

    ciclos = ciclos + 1
    If ciclos >= 40 Then ciclos = 0

End Sub

'-----------------------------------------------------------
Sub updatesNivelX

    Shared nave As nave

    Shared fotogramas As Integer
    Shared cadencia As Integer
    Shared ciclos As Integer
    Shared asteroidesAbatidos As Integer
    Shared nivel As Integer
    Shared nivel_superado As _Bit

    nave.invisible = 200
    fotogramas = fotogramas + 2
    cadencia = 0
    ciclos = 0
    nivel = nivel + 1
    asteroidesAbatidos = 0
    nivel_superado = 0

End Sub

'-----------------------------------------------------------
Sub updatesGraficos

    Shared nave As nave
    Shared titulo As titulo

    Shared astePic As Long

    nave.pic = _LoadImage(".\imgAste\playerShip.png")
    titulo.presentacion = _LoadImage(".\imgAste\msg\asteroidespresen.png")
    astePic = _LoadImage(".\imgAste\asteuropa.png")

End Sub

'-----------------------------------------------------------
Sub updatesSonido

    Shared sonido_explosion As Long
    Shared sonido_laser As Long
    Shared sonido_gameover As Long
    Shared sonido_go As Long
    Shared sonido_naveExplota As Long
    Shared sonido_superado As Long
    Shared sonido_caer As Long
    Shared sonido_vibracion As Long
    Shared sonido_levelup As Long
    Shared sonido_extralive As Long

    sonido_explosion = _SndOpen(".\sonidosAste\misc188.mp3")
    sonido_laser = _SndOpen(".\sonidosAste\disparolaser1.mp3")
    sonido_gameover = _SndOpen(".\sonidosAste\gameoveretro.mp3")
    sonido_go = _SndOpen(".\sonidosAste\gameover.mp3")
    sonido_navExplota = _SndOpen(".\sonidosAste\navexplota.mp3")
    sonido_superado = _SndOpen(".\sonidosAste\sonidoarcade1.mp3")
    sonido_caer = _SndOpen(".\sonidosAste\arcadecaer.mp3")
    sonido_vibracion = _SndOpen(".\sonidosAste\vibracion.mp3")
    sonido_levelup = _SndOpen(".\sonidosAste\levelup.mp3")
    sonido_extralive = _SndOpen(".\sonidosAste\extralive.mp3")

End Sub

'-----------------------------------------------------------
Sub updatesGenerales

    Dim a As Integer

    Shared fotogramas As Integer
    Shared comenzar As _Byte
    Shared cadencia As Integer
    Shared ciclos As Integer
    Shared puntos As Long
    Shared nivel As Integer
    Shared vidas As Integer
    Shared asteroidesAbatidos As Integer
    Shared powDisparo As Integer
    Shared record() As Long
    Shared rnivel() As Integer
    Shared nombre() As String
    Shared nivel_superado As _Bit
    Shared exit_Esc As _Bit
    Shared game_over As _Bit

    Shared nave As nave
    Shared aste() As aste
    Shared disparo() As disparo
    Shared explosion() As explosion
    Shared estrella() As estrella
    Shared estr3d() As estr3d

    fotogramas = FPS
    comenzar = 0
    cadencia = 0
    ciclos = 0
    puntos = 0
    nivel = 0
    vidas = 3
    asteroidesAbatidos = 0
    powDisparo = 7
    nivel_superado = 0
    exit_Esc = 0
    game_over = 0
    update_records record(), rnivel(), nombre()

    instancia_nave nave
    instancia_aste aste()
    instancia_disparo disparo()
    instancia_explosion explosion()
    instancia_fondoEstrellas estrella()

    For a = 1 To NRO_ESTRELLAS_3D
        instancia_Estrellas3D estr3d(), a
    Next a

End Sub

'-----------------------------------------------------------
Sub update_records (record() As Long, rnivel() As Integer, nombre() As String)

    Dim a As Integer
    Dim archivo As String

    archivo = "recordsAqb.dat"

    If _FileExists(archivo) Then
        Open archivo For Input As #1
        For a = 1 To 9
            Input #1, record(a)
            Input #1, rnivel(a)
            Input #1, nombre(a)
        Next a
        Close #1
    Else
        Print " Archivo de records  "; archivo; "  No encontrado!"
        Print " Pulse una tecla para continuar..."
        _Display
        Sleep 9
    End If

End Sub

'-----------------------------------------------------------
Sub instancia_nave (nave As nave)

    nave.rotado = 0
    nave.x = RES_X / 2
    nave.y = RES_Y / 2
    nave.radio = 20
    nave.ancho = nave.radio * 2
    nave.alto = nave.radio * 2
    nave.velX = 0
    nave.velY = 0
    nave.grados = 40 * (_Pi / 180)
    nave.viejosGrados = nave.grados
    nave.inerc = 0
    nave.vel = 1.2
    nave.velGiro = 3 * (_Pi / 180)
    nave.aceleracion = 0.2
    nave.deceleracion = 0.02
    nave.velMax = 7
    nave.avanza = 0
    nave.gira = 0
    nave.explosion = 0
    nave.duraExplosion = 180
    nave.invisible = 200

End Sub

'-----------------------------------------------------------
Sub instancia_aste (aste() As aste)

    Dim a As Integer

    For a = 1 To NRO_MAX_ASTEROIDES
        aste(a).rotado = 0
        aste(a).x = 0
        aste(a).y = 0
        aste(a).radio = 0
        aste(a).velX = 0
        aste(a).velY = 0
        aste(a).vel = 0
        aste(a).grados = 40 * (_Pi / 180)
        aste(a).activo = 0
    Next a

End Sub

'-----------------------------------------------------------
Sub instancia_disparo (disp() As disparo)

    Dim a As Integer

    For a = 1 To NRO_MAX_DISPAROS_JUGADOR
        disp(a).x = 0
        disp(a).y = 0
        disp(a).radio = 3
        disp(a).velX = 0
        disp(a).velY = 0
        disp(a).vel = 15
        disp(a).activo = 0
    Next a

End Sub

'-----------------------------------------------------------
Sub instancia_explosion (explosion() As explosion)

    Dim a As Integer
    Dim i As Integer

    For i = 1 To NRO_EXPLOSIONES_SIMULTANEAS
        For a = 1 To NRO_PARTICULAS_ESPARCIDAS
            explosion(i, a).x = 0
            explosion(i, a).y = 0
            explosion(i, a).velX = 0
            explosion(i, a).velY = 0
            explosion(i, a).ancho = 0
            explosion(i, a).duracion = 0
        Next a
    Next i

End Sub

'-----------------------------------------------------------
Sub instancia_fondoEstrellas (estrella() As estrella)

    Dim a As Integer

    For a = 1 To NRO_ESTRELLAS_FIJAS
        estrella(a).x = Int(Rnd * RES_X)
        estrella(a).y = Int(Rnd * RES_Y)

        If a < Int(NRO_ESTRELLAS_FIJAS * 60 / 100) Then
            estrella(a).ancho = 0
            estrella(a).brillo = Int(Rnd * 50) + 40

        ElseIf a < Int(NRO_ESTRELLAS_FIJAS * 85 / 100) Then
            estrella(a).ancho = 0
            estrella(a).brillo = Int(Rnd * 100) + 155

        ElseIf a < Int(NRO_ESTRELLAS_FIJAS * 95 / 100) Then
            estrella(a).ancho = 1
            estrella(a).brillo = Int(Rnd * 50) + 40

        Else
            estrella(a).ancho = 1
            estrella(a).brillo = Int(Rnd * 100) + 155
        End If
    Next a

End Sub

'-----------------------------------------------------------
Sub instancia_Estrellas3D (estr3d() As estr3d, a As Integer)

    estr3d(a).x = RES_X / 2
    estr3d(a).y = RES_Y / 2
    estr3d(a).velX = (Int(Rnd * 100) - 50) / 50
    estr3d(a).velY = (Int(Rnd * 100) - 50) / 50
    estr3d(a).ancho = 0
    estr3d(a).brillo = 90

End Sub









