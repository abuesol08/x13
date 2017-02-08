C   PROFILER.F
C THIS FILE IS USEFULL to KNOW how much time is spend running a part of code
      subroutine profiler(action,Idcad)
c     PROFILER: writes in the file "profiler.txt" the time spent between two calls to this routine 
c     Usage: call Profiler(action,IdCad)
c            action=0:6; Case 0: initialize all the counters, open the file OUTDIR\IdCad where the output will be write
c                        case 1:6: step 1) writes the time span of counter "action" 
c                                       in "profiler.txt" with its string associated "IdCad"
c                                  step 2) innitialize the counters action:6 
c            The first time 'call profiler(0,"")' to initialize the counters
c            The next times 'call profiler(action,OutCad)' will write in "profiler.txt"
c               the counter action, the string IdCad, and the time span that is in counter(action).
c     Example of use:
c       call profiler(0,"beginning")
c        ......
c       call profiler(2,"routine1");
c        ......
c       call profiler(3,"routine1 step1");
c       call profiler(3,"routine1 step2");
c       call profiler(2,"AfterCode step2");
c       call profiler(1,"AfterCode");
c
	implicit none
c     imput parameter
	integer action
      character*(*) IdCad
C   Const
	integer MaxCont
	parameter(maxCont=6)
c   Counters
	integer miliCont(maxCont)
	common /PROF/ miliCont
c   Output directory
      include 'dirt.i'
      include 'stdio.i'
      include 'units.cmn'
	character filename*80
c   External Functions
      integer ISTRLEN
	external ISTRLEN
c   Local variables
      logical lok
      integer DATE_TIME(8),i
      integer*4 seconds,miliSeconds
	character (LEN=12) Real_clock(3)
c----------------------------------------
      if (action.eq.0) then
*	  if (ISTRLEN(OUTDIR).gt.0) then
*	    filename=OUTDIR(1:ISTRLEN(OUTDIR))// '\'//IdCad
*	  else
*	    filename="output\"//IdCad
*	  endif
        filename=Cursrs(1:ISTRLEN(Cursrs))// '_' // IdCad
*        unit=openFile2(filename)
        CALL fopen(filename(1:ISTRLEN(filename)),'profiler file',
     &             'UNKNOWN',Mtprof,lok)
        if(.not.lok)Mtprof=-1
c        Mtprof=STDOUT
	else if (action.eq.-1) then
	  close(Mtprof)
	  Mtprof=-1
	  return
	endif
      call DATE_AND_TIME(real_clock(1),real_clock(2),real_clock(3),
     $               Date_time) 
c	write(*,*) IdCad," ",Real_clock(1),"  TIME:",Real_clock(2)
	Seconds=date_time(5)
	Seconds=Seconds*60+Date_time(6)
	Seconds=Seconds*60+Date_time(7)
	miliseconds=Seconds*1000+Date_time(8)
	if (action.eq.0) then
	  do  i=1,MaxCont
	    MiliCont(i)=miliSeconds
	  end do
	  if (Mtprof.gt.0) then
	    write(Mtprof,1000)
 1000     format('Counter       IdCad           miliseconds') 
	  endif
	else if (action.gt.maxCont) then
	  write(Mtprof,*) IDcad," ERROR: PROFILER IMPLEMENTED TO ",
     $        MaxCont," COUNTERS"
	else
	  if (Mtprof.gt.0) then
	    write(Mtprof,1010)
     $       Action,IdCad,miliSeconds-miliCont(action)
 1010     format(I2,5x,'"',A,'" ',I8) 
	  else
	    write(*,*) IdCad," COUNTER[",Action,"]=",
     $        miliSeconds-miliCont(action)," miliSeconds"
        endif
	  do i=action,maxCont
	    miliCont(i)=miliSeconds
	  enddo
	endif
	end 
C
C
C
*	integer function openFile2(filename)
*	implicit none
*	character*80 filename
*c     EXTERNAL FUNCTIONS
*      integer istrlen
*	external istrlen
*c     LOCAL PARAMETERS
*      integer devnum,err,Flen
*c     ---------------------------------------
*	Err=1
*	Flen=ISTRLEN(filename)
*	devnum=90
*	do while(err .ne. 0)
*	  devnum=devnum+1
*	  if (devnum .gt. 1000) then
*	    OpenFile2=-1
*          return 
*	  endif
*	  open(devnum,File=filename(1:Flen),Iostat=err)
*	end do
*	openFile2=devnum
*      end
*      