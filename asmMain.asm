TITLE MASM Template    (main.asm)
; Description:    UGH
; Authors: Abed Abualkheir, Ty Heim, Viktor Pisarenko
INCLUDE Irvine32.inc

_kbhit PROTO C
getch PROTO C



; Struct to keep track of bullets, holds their 
Proj STRUCT ; the bullets that will be fired 

    projX DWORD ? ; TO HOLD THE X LOCATION OF THE BULLET
    projY DWORD ? ; TO HOLD THE Y LOCATION OF THE BULLET

Proj ENDS

.data
    bullet Proj {-20 ,-20} ; not in the screen
    x                  DWORD 50 ; the players coordinates
    y                  DWORD 28

    stringBlank        DWORD 219 ,0 ; the player and enemy "sprites"
    stringBullet       DWORD 46 ,0 ; the bullet

    loopCount          DWORD 0

    ; AN ARRAY OF THE X LOCATIONS OF EACH ENEMY
    eXs                DWORD 0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 5, 15, 25, 35, 45, 55, 65, 75, 85, 95  ; positions of all enemy x values
    
    ; AN ARRAY OF THE Y LOCATIONS OF EACH ENEMY
    eYs                DWORD 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8 ; posistions of all enemy y values
    
    ; AN ARRAY OF EACH ENEMY'S STATUS: 1 = ALIVE, 0 = KILLED
    eA                 DWORD 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,1,1,1,1,1,1,1,1,1,1,1 ; states of enemies, if they are alive or not
    
    ; SAMPLE ENEMY FOR TESTING
    eX                 DWORD 20
    eY                 DWORD 70

    ; THE NUMBER OF ENEMIES TO PRINT TO THE SCREEN
    numEnemies         DWORD 40 ; amount of enemies

    ; SOMETHING TO KEEP TRACK OF WHICH ENEMY IS BEING LOOKED AT
    counter            DWORD 0 

    ; IF ENEMIESDEAD == NUMENEMIES THEN PLAYER WINS
    enemiesDead        DWORD 0

    ; WIN FLAG
    win                DWORD 19 ; 0 WE LOSE 1 WE WIN

    ; MESSAGES
    loseMessage        BYTE "YOU LOSE", 13, 10, 0
    winMessage         BYTE "YOU WIN", 13, 10, 0

    ; FLAG IF ENEMY WAS KILLED
    killFlag           DWORD 0

    ; BULLET THINGS
    loaded             DWORD 3 ; check if the gun is loaded or not
    hideBullet         DWORD 14 ; check if the gun is loaded or not

    ; WELCOME MESSAGE STRINGS
    welcomeMessage     BYTE " W E L C O M E  T O  S P A C E  I N V A D E R S ", 13, 10, 0
    invader1           BYTE "    ##          ##" , 13, 10, 0
    invader2           BYTE "      ##      ## ", 13, 10, 0
    invader3           BYTE "    ##############", 13, 10, 0
    invader4           BYTE "  ####  ######  ####", 13, 10, 0
    invader5           BYTE "######################", 13, 10, 0
    invader6           BYTE "##  ##############  ##  ", 13, 10, 0
    invader7           BYTE "##  ##          ##  ##", 13, 10, 0
    invader8           BYTE "      ####  ####", 13, 10, 0
    scoreStr           BYTE "SCORE: ", 13,10,0
    timeStr            BYTE "TIME:  ", 13,10,0

    ; A KEEP LOOPING FLAG
    loseFlag           DWORD 1 ; its false, you didnt lose yet

.code

; SET UP SCREEN WITH MESSAGE 
startUp PROC
    ; SET TEXT COLOR
    ; CENTER
    ; PRINT
    ; MOVE NEXT LINE
    ; ^ FOR EACH LINE OF BACKGROUND
    mov eax, lightRed 
    call SetTextColor
    mov dl, 0
    mov dh, 0

    call GoToXY
    mov edx, OFFSET scoreStr

    call WriteString 

    mov eax, white 
    call SetTextColor
    mov dl, 25
    mov dh, 0

    call GoToXY
    mov edx, OFFSET welcomeMessage

    call WriteString 

    mov eax, yellow 
    call SetTextColor
    mov dl, 0
    mov dh, 1

    call GoToXY
    mov edx, OFFSET timeStr

    call WriteString 

    mov eax, red 
    call SetTextColor

    mov dl, 35
    mov dh, 1
    call GoToXY
    mov edx, OFFSET invader1

    call WriteString 
    call crlf

    mov eax, yellow 
    call SetTextColor

    mov dl, 35
    mov dh, 2
    call GoToXY
    mov edx, OFFSET invader2

    call WriteString 
    call crlf

    mov eax, green 
    call SetTextColor

    mov dl, 35
    mov dh, 3
    call GoToXY
    mov edx, OFFSET invader3
   
    call WriteString 
    call crlf

    mov eax, lightBlue 
    call SetTextColor
    
    mov dl, 35
    mov dh, 4
    call GoToXY
    mov edx, OFFSET invader4

    call WriteString 
    call crlf

    mov eax, blue 
    call SetTextColor

    mov dl, 35
    mov dh, 5
    call GoToXY
    mov edx, OFFSET invader5

    call WriteString 
    call crlf

    mov eax, magenta 
    call SetTextColor

    mov dl, 35
    mov dh, 6
    call GoToXY
    mov edx, OFFSET invader6

    call WriteString 
    call crlf

    mov eax, lightRed 
    call SetTextColor

    mov dl, 35
    mov dh, 7
    call GoToXY
    mov edx, OFFSET invader7

    call WriteString 
    call crlf

    mov eax, red
    call SetTextColor

    mov dl, 35
    mov dh, 8
    call GoToXY
    mov edx, OFFSET invader8

    call WriteString 
    call crlf

   
    ret
startUp ENDP



darkDraw PROC ; this is for the "clear screen" effect but its much much faster than the CALL Clrscr
    
    ; SET COLOR TO BLACK
    ; GO TO LAST POSITION
    ; DRAW BLACK SQUARE

    mov eax, black
    call SetTextColor
    mov dl, BYTE ptr x
    mov dh, BYTE ptr y
    call GoToXY
    mov edx, OFFSET stringBlank

    call WriteString 

    ret
darkDraw ENDP

darkDrawE MACRO ex1:REQ, ey1:REQ ; this is for the "clear screen" effect but its much much faster than the CALL Clrscr
        push eax
        push edx
        push ebx
        push ecx

    ; SET COLOR TO BLACK
    ; GO TO LAST POSITION
    ; DRAW BLACK SQUARE

    mov eax, black
    call SetTextColor
    mov dl, BYTE ptr ex1
    mov dh, BYTE ptr ey1
    call GoToXY
    mov edx, OFFSET stringBlank

    call WriteString 

         pop ecx
        pop ebx
        pop edx
        pop eax

ENDM

draw PROC ; draw the players ship
        push eax
        push edx
        push ebx
        push ecx
    ; GO TO (X,Y)
    ; DRAW BLOCK
    mov dl, BYTE ptr x
    mov dh, BYTE ptr y
    call GoToXY
    mov edx, OFFSET stringBlank

    call WriteString 

        pop ecx
        pop ebx
        pop edx
        pop eax
    ret

draw ENDP

playerHitEnemy MACRO ex:REQ, ey:REQ ; requires enemy x and y values as parameters
    ; CHECK IF BULLET'S X,Y IS EQUAL TO ENEMY X,Y
    push eax

    mov eax, bullet.projX ; check if the X values are the same
    cmp eax, ex
        je equalX ; IF EQUAL CHECK Y

        jmp quitP ; IF NOT EQUAL, QUIT THE CHECK

    equalX:   ; check if the Y values are the same or the same
        mov eax, bullet.projY
        cmp eax, ey
            je equalY ; IF EY AND Y ARE EQUAL THEN HIT
        inc eax
        cmp eax, ey
            je equalY ; IF EY AND Y+1 ARE EQUAL THEN HIT
        dec eax
        dec eax
        cmp eax, ey 
            je equalY ; IF EY AND Y-1 ARE EQUAL THEN HIT
            
        jmp quitP ; IF NOT EQUAL, QUIT THE CHECK


    equalY: ; if this runs that means that they collide, print the enemies in black color and change their "status" to dead
        mov eax, white 
        call setTextColor 
        mov killFlag, 1 ; SET KILL FLAG TO CHANGE THE ENEMY'S STATUS TO 0

    quitP: 


    pop eax 
ENDM

; JUST CODE WE HAD A LOT THAT MADE SENSE TO TURN INTO A PROCEDURE
inc4 PROC
    inc counter
    inc counter
    inc counter
    inc counter

    ret
inc4 endp


darkDrawBullet MACRO ex1:REQ, ey1:REQ ; this is for the "clear screen" effect but its much much faster than the CALL Clrscr
        push eax
        push edx
        push ebx
        push ecx

    mov dl, BYTE ptr ex1
    mov dh, BYTE ptr ey1
    call GoToXY
    mov eax, black
    call SetTextColor
    mov edx, OFFSET stringBullet
    call WriteString  


        pop ecx
        pop ebx
        pop edx
        pop eax

ENDM

drawBullet PROC ; draws the bullet
    push ebx
    push eax
    push ecx
    push edx 

    .while bullet.projY > 0 ; keep the bullet moving upward until it hits out of the screen

    mov ecx, numEnemies
    mov counter, 0
    checkCollisions: ; check if the bullet is hitting any enemy 
        mov ebx, counter
        playerHitEnemy eXs[ebx], eYs[ebx]
        mov eax, killFLag
        cmp eax, 1
            JE hit ; BULLET AND ENEMY AT SAME LOCATION
        jmp continue ; DIFFERENT LOCATION
        hit:
            mov eA[ebx], 0 ; SET ENEMY STAT TO DEAD
            mov killFlag, 0 ; RESET KILL FLAG
            inc enemiesDead ; ADD AN ENEMY TO DEAD COUNT
            jmp endColCheck ; JUMP OUT OF COLISION CHECK
        continue:
            call inc4 ; GO TO NEXT ENEMY

    LOOP checkCollisions ; LOOP THROUGH EVERY ENEMY TIL ONE IS HIT OR NONE ARE HIT

    ;MOVE BULLET FORWARD AND PRINT
    mov dl, BYTE ptr bullet.projX 
    mov dh, BYTE ptr bullet.projY
    call GoToXY
    mov eax, yellow
    call SetTextColor
    mov edx, OFFSET stringBullet
    call WriteString ; 
 
    dec bullet.projY 

    .endw ; UNTIL BULLET HITS OR IS OUT OF SCREEN
    endColCheck:


    pop edx
    pop ecx
    pop eax
    pop ebx

    ret
drawBullet ENDP

drawEnemy MACRO ex:REQ, ey:REQ, ea:REQ ; draws the enemy ships at certain x and y coords and if they are alive
        push eax
        push edx
        push ebx
        push ecx

        cmp ey, 30 ; check if the enemy y value is past the bottom of the map (you lose)
            jne keepGoing
        mov loseFlag, 0


        keepGoing:

        mov eax, ea ; CHECK IF ENEMY IS ALIVE
        cmp eax, 1
            JE d ; ENEMY ALIVE
        jmp endD ; ENEMY DEAD
        
        d:
            inc ex ;MOV TO THE RIGHT
            mov eax, 100 ;BOUND
            CMP  eax, ex ; CHECK IF AT BOUND
                JE moveDown ; AT BOUND
            JMP noJump ; NOT AT BOUND
            moveDown:
                mov ex, 0 ; MOVE TO FAR LEFT
                inc ey ; MOVE DOWN A LINE
            noJump:
                ; PRINT ENEMY
                mov dl, BYTE ptr ex 
                mov dh, BYTE ptr ey
                mov eax, Blue
                call SetTextColor
                call GoToXY
                mov edx, OFFSET stringBlank
                call WriteString 
          endD:
        pop ecx
        pop ebx
        pop edx
        pop eax

ENDM

checkEnemiesAtBottom PROC
    push eax
    push edx
    push ebx
    push ecx

    mov counter, 0
    mov ecx, numEnemies
    
    ; IF AN ENEMY IS AT BOTTOM THEN PLAYER LOSES

    check:
    mov ebx, counter
    mov eax, eYs[ebx]
    cmp eax, 28
        JE lose

    LOOP check
    jmp exitCheck
    lose: 
        mov win, 0
        mov eax, green
        call SetTextColor
        mov eax, OFFSET loseMessage
        call WriteString
    exitCheck:

    pop ecx
    pop ebx
    pop edx
    pop eax
checkEnemiesAtBottom endp

asmMain PROC C
    call startUp

    mov ebx, 12
    .while ebx > 0

    ; check if enemies killed = total enemies
    mov eax, numEnemies
    cmp enemiesDead, eax ; --------------- set the amount of enemies u need to kill to win
        jge endGame

    cmp loseFlag, 0
        je gameOver

    jmp goOn

    endGame:; Set up a win screen
         call Clrscr
         mov win, 1
         mov eax, green
         call SetTextColor
         mov edx, OFFSET winMessage
         call WriteString
         mov ebx, 0
         jmp quit


    gameOver:
         call Clrscr
         mov eax, red
         call SetTextColor
         mov edx, OFFSET loseMessage
         invoke WriteString
         mov ebx, 0
         jmp quit

    goOn:

    cmp hideBullet, 0
        jle hideIt
        jmp dontHide

        hideIt:
        .while bullet.projY < 30
            darkDrawBullet bullet.projX, bullet.projY
            inc bullet.projY
        .endw

    dontHide:

    dec loaded ; decrease the amount of "time" that has passed 
    dec hideBullet

    

    mov eax, 50 ; SO ENEMIES DONT MOVE HECKA QUICK
    call Delay
    
    ; **** PRINT SCORE ****
    mov dl, 6
    mov dh, 0
    mov eax, Green
    call SetTextColor
    call GoToXY
    mov eax, enemiesDead
    call writeInt

    ; **** PRINT TIME ****
    mov dl, 6
    mov dh, 1
    mov eax, white
    call SetTextColor
    call GoToXY
    mov eax, loopCount
    call writeInt

    inc loopCount

    ;CHECK IF KEY HIT
    call _kbhit
    CMP eax, 0
        JNE key ; KEY HIT
    JMP noKey ; NO KEY HIT

    key:
        call getch ; FIND OUT WHICH KEY WAS HIT

        CMP eax, 32 ; SPACE BAR
            JE space
        CMP eax, 119 ; W
            JE up
        CMP eax, 97 ; A
            JE left
        CMP eax, 115 ; S
            JE down
        CMP eax, 100 ; D
            JE right
        JMP noKey ; IF NOT AN ELIGIBLE KEY, GO TO NO KEY

    space: ;if the space key is pressed... move a projectile to be on top of the player and then move that projectile upward
        push eax
        cmp loaded, 0; check if the gun is loaded
        jle shoot
        jmp noKey

        
        shoot:
        mov loaded, 8 ; unload the gun... the 15 is the amount of "time" before you can fire again
        mov hideBullet, 7; reset hiding the trail 

        mov eax, x
        mov bullet.projX, eax
        dec y
        mov eax, y
        mov bullet.projY, eax
        CALL drawBullet
        inc y
        pop eax
        JMP noKey


    up:
        call darkDraw
        mov eax, y
        dec eax
        CMP eax, 0 ; IF AT BORDER DONT MOVE OFF SCREEN
            JLE noKey
        mov y, eax
        JMP noKey

    down:
        call darkDraw
        mov eax, y
        inc eax
        CMP eax, 30 ; IF AT BORDER DONT MOVE OFF SCREEN
            JGE noKey
        mov y, eax
        JMP noKey
    left:
        call darkDraw
        mov eax, x
        dec eax
        CMP eax, 0 ; IF AT BORDER DONT MOVE OFF SCREEN
            JLE noKey
        mov x, eax
        JMP noKey
    right:
        call darkDraw
        mov eax, x
        inc eax
        CMP eax, 100 ; IF AT BORDER DONT MOVE OFF SCREEN
            JGE noKey
        mov x, eax
        JMP noKey
    
  
    noKey:
        ; DRAW PLAYER
        call draw

        ; LOOP THROUGH ALL ENEMIES
        mov ecx, numEnemies
        mov counter, 0

        ; DRAW OVER LAST ENEMIES
        drawXs:
            mov ebx, counter
            darkDrawE eXs[ebx], eYs[ebx]
           call inc4

        LOOP drawXs
     
        mov ecx, numEnemies
        mov counter, 0

        ; DRAW MOVED ENEMIES
        draw0s:
            mov ebx, counter
            drawEnemy eXs[ebx], eYs[ebx], eA[ebx];, edx
            call inc4

        LOOP draw0s

        mov ecx, numEnemies
        mov counter, 0

        ; CHECK EACH ENEMY FOR DEATH, IF DEAD MOVE OFF SCREEN SO BULLET CANT HIT IT
        checkD:
            mov ebx, counter
            mov eax, eA[ebx]
            cmp eax, 0
                JE Dead
            jmp next
            Dead:
                mov eYs[ebx], 10000
                mov eXs[ebx], 10000
            next:
            call inc4

        LOOP checkD
        
        call checkEnemiesAtBottom
    .endw

    quit:


    mov eax, 1000
    call Delay

    ret
asmMain ENDP

end