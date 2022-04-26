program wumpus

   ! Program: Hunt the Wumpus
   ! Author: Nathaniel Johnson
   ! Course: CIS*3190
   ! Purpose: A recreation of the classic game 'Hunt the Wumpus'
   ! New Fetures
   ! Added an ASCII representation of the room, where X is the player
   ! Added an Arrow Counter to let the player know how many arrows they have left
   ! Added TWO new events.
   ! The first event is a torch, that when found, can be thrown into a room to scout ahead. The player loses the torch
   ! after throwing it, and cannot retrieve it. Thus, it is a single use item.
   ! The second event is a new monster, called the Luminous Snaggler, which will eat you the moment you enter its room
   ! Both the torch and the Luminous Snaggler emit a faint light, and thus have the same hazard message of
   ! "There is a faint glow emitting from room X"
   ! While the torch is a useful item, it may not be worth the risk of trying to obtain it!

   implicit none
   character(len=1) :: user_input
   integer, dimension(20, 3) :: map
   integer, dimension(8) :: L, M ! L is the Hazard List
   integer :: arrows, player_location, end_flag, reset_flag, i
   integer :: has_torch, found_torch, valid_input_flag
  

   reset_flag = 0
   do while (user_input /= 'N' .and. user_input /= 'n' .and. user_input /= 'Y' .and. user_input /= 'y')
      write (*,*) 'Instructions? (Y-N)'
      read(*,*) user_input
   end do

   !Game setup
   call getInstructions(user_input)
   ! call announceSequals()
   call loadMap(map)
   call loadEvents(L, M, arrows, has_torch, found_torch)
   arrows = 5
   end_flag = 0
   player_location = L(1)
   write (*,*) 'Hunt the Wumpus'

   ! Game loop
   do while (end_flag == 0)
      ! Enable this if you would like cheats
      ! write (*,*) M !-- Cheat Code

      ! If playing again with the same setup, this loop runs to reset the game state
      if (reset_flag == 1) then
         arrows = 5
         i = 1
         do while (i < 9) 
            L(i) = M(i)
            i = i + 1
         end do
         player_location = L(1)
         reset_flag = 0
      else if (end_flag /= 0) then
         call loadEvents(L, M, arrows, has_torch, found_torch)
      end if

      !Each turn
      call drawRoom(map, L)
      call checkHazards(map, L, found_torch)
      write (*,*) ''
      write (*,*) 'You are in room ', player_location
      write (*,*) 'Tunnels lead to ', map(player_location, 1), " ", map(player_location, 2), " ", map(player_location, 3)
      write (*,*) 'Arrows remaining:', arrows
      write (*,*) ' '
      valid_input_flag = 0

      do while (valid_input_flag == 0)
         if (has_torch == 0) then
            write (*,*) 'Shoot, Move, or Exit (S-M-E)?'
         else
            write (*,*) 'Shoot, Move, Throw Torch, or Exit (S-M-T-E)?'
         end if
         read (*,*) user_input
         !Moving
         if (user_input == 'm' .or. user_input == 'M') then
            valid_input_flag = 1
            call movePlayer(map, L, player_location)
            call tryHazards(map, L, player_location)
            call tryTorch(L, player_location, has_torch, found_torch)
            
         !Shooting
         else if (user_input == 's' .or. user_input == 'S') then
            valid_input_flag = 1
            call fireArrow(map, L, end_flag, reset_flag, arrows)
            call checkArrowLoss(arrows, end_flag, reset_flag)
            
         !Throwing Torch
         else if ((user_input == 't' .or. user_input == 'T') .and. has_torch == 1) then
            valid_input_flag = 1
            call throwTorch(map, L, has_torch)
            
         !Exiting the game
         else if (user_input == 'e' .or. user_input == 'E') then
            valid_input_flag = 1
            write (*,*) 'Hee Hee Hee! You run out of the cave crying!'
            end_flag = -2
         end if
      end do


   end do
   write(*,*) 'Thanks for playing!'



   contains

   

   ! subroutine announceSequals()
   !    write( *,* ) 'There will be no sequels, not in Fortran anyway..'
   ! end subroutine announceSequals

   
   subroutine loadMap(map)
      ! loads all the room's doors
      !   this is probably the worst way to do it but I don't see a pattern
      integer, dimension(20, 3), intent(inout) :: map
      map(1, 1:3) = (/2, 5, 8/)  ! room 1
      map(2, 1:3) = (/1, 3, 10/) ! room 2
      map(3, 1:3) = (/2, 4, 12/) ! room 3
      map(4, 1:3) = (/3, 5, 14/) ! room 4
      map(5, 1:3) = (/1, 4, 6/) ! room 5

      map(6, 1:3) = (/5, 7, 15/) ! room 6
      map(7, 1:3) = (/6, 8, 17/) ! room 7
      map(8, 1:3) = (/1, 7, 9/) ! room 8
      map(9, 1:3) = (/8, 10, 18/) ! room 9
      map(10, 1:3) = (/2, 9, 11/) ! room 10

      map(11, 1:3) = (/10, 12, 19/) ! room 11
      map(12, 1:3) = (/3, 11, 13/) ! room 12
      map(13, 1:3) = (/12, 14, 20/) ! room 13
      map(14, 1:3) = (/4, 13, 15/) ! room 14
      map(15, 1:3) = (/6, 14, 16/) ! room 15

      map(16, 1:3) = (/15, 17, 20/) ! room 16
      map(17, 1:3) = (/7, 16, 18/) ! room 17
      map(18, 1:3) = (/9, 17, 19/) ! room 18
      map(19, 1:3) = (/11, 18, 20/) ! room 19
      map(20, 1:3) = (/13, 16, 19/)! room 20
   end subroutine loadMap

   ! Assigns each event and the item to a random room
   ! No hazards or item will occupy the same room
   subroutine loadEvents(L, M, arrows, has_torch, found_torch)
      implicit none
      integer :: j, k
      integer, intent(inout) :: arrows, has_torch, found_torch
      integer, dimension(8), intent(out) :: L, M
      ! Locate L Array Items
      j = 1
      arrows = 5
      has_torch = 0
      found_torch = 0
      do while (j < 9) 
         L(j) = randomA()
         M(j) = L(j)
         j = j + 1
      end do

      !Check for crossovers
      j = 1
      do while (j < 9)
         k = j
         do while (k < 9)
            if (j /= k) then
               do while (L(j) == L(k))
                  L(j) = randomA()
                  M(j) = L(j)
                  j = 0
                  k = 15
               end do
            end if
            k = k+1
         end do
         j = j+1
      end do
                  
   end subroutine loadEvents

   ! creates a random number between 1 and 4 inclusive
   ! used when picking a random door
   integer function randomC()
      implicit none
      real :: c
      call random_number(c)
      c = c*4 + 1
      randomC = int(c)
      ! write (*,*) c
   end function randomC

   ! creates a random number between 1 and 3 inclusive
   ! used when picking a random door
   integer function randomB()
      implicit none
      real :: b
      call random_number(b)
      b = b*3 + 1
      randomB = int(b)
      ! write (*,*) b
   end function randomB

   ! creates a random number between 1 and 20 inclusive
   integer function randomA()
      implicit none
      real :: a
      call random_number(a)
      a = a * 20 + 1
      ! write (*,*) 'a = '
      ! write (*,*) a
      randomA = int(a)
   end function randomA

   !Ensures that the input is an iteger equal to or greater than 0
   integer function getValidInteger()
      implicit none
   ! Adapted from: https://stackoverflow.com/questions/22073853/checking-input-type-in-fortran-read-statement
      integer :: error, input, number_flag, valid_flag
      number_flag = 0
      valid_flag = 0
      do while (valid_flag == 0)
         do while (number_flag == 0)
            read(*,'(i10)',iostat=error) input

            if ( error == 0 ) then
               number_flag = 1
            else 
               write(*,*) 'That''s not a number! Try again.'
            end if
         
         end do

         if (input < 1) then
            write (*,*) 'Not possible. Try Again'
            number_flag = 0
         else 
            valid_flag = 1
         end if
      end do
      getValidInteger = input
   end function getValidInteger

   !Checks the endgame flag to determine if the game is over
   !Displays appropriate message, and runs the post game setup
   subroutine endGame(end_flag, reset_flag)
      implicit none
      integer, intent(out) :: end_flag, reset_flag
      character(len=1) :: isRepeat, isAgain
      integer :: i

      if (end_flag /= 0) then
         if (end_flag == 1) then
            write (*,*) 'Hee Hee Hee - The Wumpus''ll get you next time!!'
         else if (end_flag == -1) then
            write (*,*) 'HA HA HA - You Lose!'
         end if

            i = 1
         do while (i < 9)
            L(i) = M(i)
            i = i + 1
         end do

         do while (end_flag /= 0)
            write (*,*) 'Same set up (Y-N)?'
            read (*,*) isRepeat
            if (isRepeat == 'Y' .or. isRepeat == 'y') then
               reset_flag = 1
               end_flag = 0
            else if (isRepeat == 'N' .or. isRepeat == 'n') then
               end_flag = 0
               reset_flag = 0
            end if
         end do

         do while (reset_flag == 0) 
            write (*,*) 'Play again (Y-N)?'
            read (*,*) isAgain

            if (isAgain == 'Y' .or. isAgain == 'y' ) then
               end_flag = 0
               reset_flag = 1
            else if (isAgain == 'N' .or. isAgain == 'n') then
               end_flag = 2
               reset_flag = 1
            end if
         end do
      end if
      ! play again?
   end subroutine endGame

   !Checks the adjacent rooms and displays the appropriate
   !hazard or item hints
   subroutine checkHazards(map, L, found_torch)
      implicit none
      integer :: j, k
      integer, dimension(20, 3), intent(in) :: map
      integer, dimension(8), intent(in) :: L
      integer, intent(in) :: found_torch
      j = 2
      do while (j < 9)
         k = 1
         do while (k < 4)
            ! check each of the possible exits to see if they contain a hazard
            if (map(L(1), k) == L(j)) then
               if (j == 2) then
                  write (*,*) 'I smell a Wumpus!'
               else if (j == 3 .or. j == 4) then
                  write (*,*) 'I feel a draft'
               else if (j == 5 .or. j == 6) then
                  write (*,*) 'Bats nearby!'
               else if (j == 7 .or. (j == 8 .and. found_torch == 0)) then
                  200 format (a, i2)
                  write (*, 200) 'There is a faint light eminating from room ', map(L(1), k)
               end if
            end if
            k = k + 1;
         end do
         j = j + 1;
      end do
   end subroutine checkHazards

   !Checks to see if your arrow killed the wumpus or yourself
   subroutine checkKill(arrow_location, L, end_flag)
      implicit none
      integer, intent(in) :: arrow_location
      integer, intent(inout) :: end_flag
      integer, dimension(8), intent(in) :: L

      !! Check if the wumpus has been terminated
      ! write (*,*) 'CHECKKILL: Arrow: ', arrow_location
      if (arrow_location == L(2)) then
         end_flag = 1
         write (*,*) "Aha! You got the Wumpus!"

      !! Check if you have shot yourself in the bum
      else if (arrow_location == L(1)) then
         ! write (*,*) 'Arrow: ', arrow_location, ' You: ', L(1)
         end_flag = -1
         write (*,*) "Ouch! Arrow got you!"
      end if
   end subroutine checkKill

   !handles the building of the arrow path, and the firing of the arrow
   subroutine fireArrow(map, L, end_flag, reset_flag, arrows)
      implicit none
      integer :: J9, k, k1, isBadArrow, arrow_location
      integer, dimension(5) :: P_arrowpath
      integer, dimension(8), intent(in) :: L
      integer, dimension(20, 3), intent(in) :: map
      integer, intent(inout) :: end_flag, arrows, reset_flag
      P_arrowpath = 0
      J9 = 0
      ! Path of the Arrow
      do while (J9 < 1 .or. J9 > 5)
         write (*,*) 'No. of rooms (1-5)'
         J9 = getValidInteger()
         if (J9 < 1 .or. J9 > 5) then
            write (*,*) 'Not between 1 and 5. Try again.'
         end if
      end do

      k = 1
      do while (k < J9 + 1)
         isBadArrow = 0
         write (*,*) "Room #"
         ! TODO : BETWEEN 1 - 20 Check
         P_arrowpath(k) = getValidInteger()
         if (L(1) == P_arrowpath(1)) then
            write (*,*) 'You shouldn''t shoot yourself.'
            isBadArrow = 1

         else if (L(1) == P_arrowpath(2)) then
            write (*,*) 'Arrows aren''t that crooked - try another room'
            isBadArrow = 1
         end if
         if (k > 2) then
            if (P_arrowpath(k) == P_arrowpath(k-2)) then
               write (*,*) 'Arrows aren''t that crooked - try another room'
               isBadArrow = 1
            end if
         end if

         if (isBadArrow == 0) then
            k = k+1
         end if
      end do
   
      ! Shoot Arrow
      ! write (*,*) 'Arrow Path: ', P_arrowpath
      arrows = arrows - 1
      arrow_location = L(1)

      k = 1
      do while (k < J9 + 1)
         k1 = 1
         do while (k1 < 4)
            ! Check if the arrow can follow the path
            if (map(arrow_location, k1) == P_arrowpath(k)) then
               ! advance the arrow
               arrow_location = P_arrowpath(k)
               call checkKill(arrow_location, L, end_flag)
               ! no need to check the remaining doors, just go to the next room
               k = k + 1
               k1 = 1
            else ! No tunnel for arrow
               k1 = k1 + 1
            end if
             
         end do
         ! if we look at every door and the one we chose dne,
         ! choose a random door to follow
         if (k < J9) then
            arrow_location = map(arrow_location, randomB())
            call checkKill(arrow_location, L, end_flag)
         end if 
         k = k + 1
      end do

      if (end_flag == 0) then
         write (*,*) 'Missed'
      end if
      call endGame(end_flag, reset_flag)
   end subroutine fireArrow

   !Checks to see if you have run out of arrows
   subroutine checkArrowLoss(arrows, end_flag, reset_flag)
      implicit none
      integer, intent(inout) :: arrows, end_flag, reset_flag
      if (end_flag == 0) then
         if (arrows < 1) then
            end_flag = -1
            call endGame(end_flag, reset_flag)
         end if
      end if
   end subroutine checkArrowLoss

   ! Moves the Wumpus
   subroutine moveWumpus(map, L)
      implicit none
      integer :: k
      integer, dimension(8), intent(inout) :: L
      integer, dimension(20, 3), intent(in) :: map

      k = randomC()

      if (k /= 4) then
         L(2) = map(L(2), k)
      end if

      if (L(2) == L(1)) then
            write (*,*) 'TSK TSK TSK - Wumpus got you'
            end_flag = -1
            call endGame(end_flag, reset_flag)
         end if

   end subroutine moveWumpus

   !Checks to see if the user is in the same room as a hazard
   !Displays the appropriate error message, and performs the 
   !associated hazard action
   recursive subroutine tryHazards(map, L, player_location)
      implicit none
      integer, dimension(20, 3), intent(in) :: map
      integer, intent(inout) :: player_location
      integer, dimension(8), intent(inout) :: L
      ! Wumpus
      if (player_location == L(2)) then
         write (*,*) '. . .Oops! I bumped into a Wumpus!'
         call moveWumpus(map, L)

      else if (player_location == L(3) .or. player_location == L(4)) then
         write (*,*) 'YYYIIIIEEEE . . . Fell in a pit'
         end_flag = -1
         call endGame(end_flag, reset_flag)

      else if (player_location == L(5) .or. player_location == L(6)) then
         write (*,*) 'ZAP--Super Bat Snatch! Elesewhereville for you!'
         L(1) = randomA()
         player_location = L(1)
         call tryHazards(map, L, player_location)

      else if (player_location == L(7))then
         write (*,*) 'You enter the room and approach the glowing light...'
         write (*,*) 'FWOMP! -- From above you are snatched and devoured by a Luminous Snaggler!'
         end_flag = -1
         call endGame(end_flag, reset_flag)
      end if
   end subroutine tryHazards

   !Checks to see if you can pick up the torch
   subroutine tryTorch( L, player_location, has_torch, found_torch)
      implicit none
      integer, intent(inout) :: player_location, has_torch, found_torch
      integer, dimension(8), intent(inout) :: L

      if (found_torch == 0 .and. player_location == L(8)) then
         write (*,*) 'You enter the room and approach the glowing light...'
         write (*,*) '..and find a torch!'
         found_torch = 1
         has_torch = 1
      end if
   end subroutine tryTorch

   !The routine for moving the player around the map
   !Handles user input, and updates the players location
   subroutine movePlayer(map, L, player_location)
      implicit none
      integer, intent(inout) :: player_location
      integer, dimension(8), intent(inout) :: L
      integer, dimension(20, 3), intent(in) :: map
      integer :: f, k, new_move
      integer :: isLegal
      f = 0
      new_move = 0
      isLegal = 0

      ! Check if the move is within range and legal
      do while (isLegal == 0)
         write (*,*) 'Where to?'
         new_move = getValidInteger()
         if (new_move >= 1 .and. new_move <= 20) then
            k = 1
            ! check if the move is legal
            do while (k < 4)
               if (map(new_move, k) == player_location) then
                  ! write (*,*) "player location", player_location, "map ", map(new_move, k), "new move", new_move, "k", k
                  isLegal = 1
               end if
               k = k + 1
            end do
               if (isLegal == 0) then 
                  write (*,*) 'Not possible -'
               end if
         else
            write (*,*) 'Not possible -'
         end if
      end do

      L(1) = new_move
      player_location = L(1)

   end subroutine movePlayer

   !routine for throwing the torch into the room
   !handles the user input
   subroutine throwTorch(map, L, has_torch)
      implicit none
      integer, dimension(8), intent(inout) :: L
      integer, dimension(20, 3), intent(in) :: map
      integer, intent(inout) :: has_torch
      integer :: roomNumber, k
      do while (has_torch == 1)
         write(*,*) 'Into which room will you throw the torch?'
         roomNumber = getValidInteger()
         k = 1
         do while (k < 4)
            if (map(L(1), k) == roomNumber) then
               k = 5
               write (*,*) 'You throw the torch into the room and...'
               has_torch = 0
               call scoutRoom(L, roomNumber)
            end if
            k = k+1
         end do
         if (has_torch == 1) then
           write(*,*) "Not a valid room."
         end if
      end do
   end subroutine throwTorch

   !called by throwTorch
   !displays the appropriate hint message for
   !the room the torch has been tossed in to
   subroutine scoutRoom(L, roomNumber)
      implicit none
      integer, dimension(8), intent(inout) :: L
      integer, intent(in) :: roomNumber

      if (roomNumber == L(2)) then
         !!wumpus
         write (*,*) '...you see the light go out...'
         write (*,*) '...and you hear the Wumpus begin to wake!'
         call moveWumpus(map, L)
      else if (roomNumber == L(3) .or. roomNumber == L(4)) then
         !!bottomless pit
         write(*,*) '...you see the light go out...'
         write(*,*) '...but you don''t hear the torch hit the ground.'
      else if (roomNumber == L(5) .or. roomNumber == L(6)) then
         !! bats
         write(*,*) '...and the screeching of the bats becomes frenzied.'
      else if (roomNumber == L(7)) then
         write(*,*) '...you see the light go out...'
         write(*,*) '...and then re-appear!'
      else
         write(*,*) '...you see the light go out...'
         write(*,*) '...and nothing happens.'
      end if
   end subroutine scoutRoom

   !displays the instructions if the user has asked for them
   subroutine getInstructions(user_input)
      implicit none
      character(len=1), intent(in) :: user_input
      if (user_input == 'y' .or. user_input == 'Y') then
         write (*,*) 'Welcome to ''Hunt The Wumpus'''
         write (*,*) 'The Wumpus lives in a cave of 20 Rooms. Each room'
         write (*,*) 'has 3 tunnels leading to other rooms. (Look at a'
         write (*,*) 'Dodecahefron to see how this works-if you don''t know'
         write (*,*) 'What a Didecahedron is, google it)'
         write (*,*) 
         write (*,*) 'Items:'
         write (*,*) 'Torch - A single use item that can be discovered while exploring'
         write (*,*) 'the cave system. It can be thrown into a room to scout ahead.'
         write (*,*)
         write (*,*) 'Hazards:'
         write (*,*) 'Bottomless Pits - Two rooms have bottomless pits in them.'
         write (*,*) '     If you go there, you fall into the pit (& lose!)'
         write (*,*) 'Super Bats - Two other rooms have Super Bats. If you'
         write (*,*) 'go there, a bat grabs you and takes you to some other'
         write (*,*) 'at random. (Which might be troublesome)'
         write (*,*) 'Luminous Snaggler - One other room will contain a '
         write (*,*) 'Luminous Snaggler, which emmits a faint glow from it''s lure.'
         write (*,*) 'Entering the room will result in a swift death (& a game over!)'
         write (*,*) 
         write (*,*) '     Wumpus:'
         write (*,*) 'The Wumpus is not bothered by the hazarrds (he has sucker'
         write (*,*) 'feet and is too big for a bat to lift or the snaggler to eat). '
         write (*,*) 'Usually he is asleep. Three things wake him up: Your entering'
         write (*,*) 'his room, shooting an arrow, or your throwing your torch into'
         write (*,*) 'his room.'
         write (*,*) '     If thie Wumpus wakes, he moves (p=.75) one room'
         write (*,*) 'or stays still (p=.25). After that, if he is where you'
         write (*,*) 'are, he eats you up (& you lose!)'
         write (*,*) 
         write (*,*) '     You:'
         write (*,*) 'Each turn you may move or shoot a crooked arrow'
         write (*,*) '     Moving: You can go one room (Thru one tunnel)'
         write (*,*) '     Arrows: You have 5 Arrows. You lose when you run out'
         write (*,*) '     Each arrow can go from 1 to 5 rooms. You aim by telling'
         write (*,*) '     the computer the room #s you want the arrow to go to.'
         write (*,*) '     If the Arrow can''t go that way (ie no tunnel) it moves'
         write (*,*) '     at random to the next room'
         write (*,*) '        If the arrow hits the Wumpus, you win.'
         write (*,*) '        If the arrow hits you, you lose.'
         write (*,*) 
         write (*,*) '     Warnings:'
         write (*,*) '        When you are one room away from the Wumpus or a Hazard'
         write (*,*) '        The computer says:'
         write (*,*) 'Wumpus   - ''I smell a Wumpus!'''
         write (*,*) 'Bat      - ''Bats nearby!'''
         write (*,*) 'Pit      - ''I feel a draft'''
         write (*,*) 'Snaggler - ''TThere is a faint light eminating from room X'
         write (*,*) 'Torch    - ''TThere is a faint light eminating from room X'
         write (*,*) 
      end if
   end subroutine getInstructions

   !draws the room, doors, and player character
   subroutine drawRoom(map, L)
      implicit none
      integer, dimension(8), intent(in) :: L
      integer, dimension(20, 3), intent(in) :: map
   
   100 format(i2, 19x, i2)
   101 format(a11, i2, a)
   
      write (*,*) '----------------------'
      write (*,*) '|                    |'
      write (*,*) '|                    |'
      write (*,*) '|                    |'
      write (*,100) map(L(1),1), map(L(1),2)
      write (*,*) '|         X          |'
      write (*,*) '|                    |'
      write (*,*) '|                    |'
      write (*,101) ' ----------', map(L(1), 3), '----------'

   end subroutine drawRoom
   
end program