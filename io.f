c ---------------------------------------------------------------------
      subroutine write_soln
c
      include 'MYDATA'

      character*15 fnamec,fnamen

      integer icalld
      save    icalld
      data    icalld  /-1/


      icalld = icalld + 1
      write (6,*) 'iostep: ',icalld,time,dt

      write(fnamec,'(A6,I5.5,A4)') 'soln_c', icalld, '.vtk'
      open(unit=72,file=fnamec)

      write(72,'(A26)') '# vtk DataFile Version 3.0'
      write(72,*) '2D scalar data'
      write(72,*) 'ASCII'
      write(72,*) ''

      write(72,*) 'DATASET UNSTRUCTURED_GRID'
      write(72,*) 'POINTS',' ', nn,' ',' float'
      do i=1,nn
         write(72,*) nodel(1,i),nodel(2,i),0.
      enddo
      write(72,*) ''

      write(72,*) 'CELLS ', nel, nel*5
      do i=1,nel
         if (elml(9,i) .eq. 3) then
            write(72,*) 
     >        elml(9,i),elml(1,i)-1,elml(2,i)-1,elml(3,i)-1
         elseif (elml(9,i) .eq. 4) then
            write(72,*) 
     >        elml(9,i),elml(1,i)-1,elml(2,i)-1,elml(3,i)-1,elml(4,i)-1
         endif
      enddo
      write(72,*) ''

      write(72,*) 'CELL_TYPES', nel
      do i=1,nel
         if (elml(9,i) .eq. 3) write(72,*) 5
         if (elml(9,i) .eq. 4) write(72,*) 9
      enddo
      write(72,*) ''

      write(72,*) 'CELL_DATA',nel
      write(72,*) 'SCALARS  density  float 1'
      write(72,*) 'LOOKUP_TABLE default'
      do i=1,nel
         write(72,*) u(i,1)
      enddo
      write(72,*) 'SCALARS ru float 1'
      write(72,*) 'LOOKUP_TABLE default'
      do i=1,nel
         write(72,*) u(i,2)
      enddo
      write(72,*) 'SCALARS rv float 1'
      write(72,*) 'LOOKUP_TABLE default'
      do i=1,nel
         write(72,*) u(i,3)
      enddo
      write(72,*) 'SCALARS re float 1'
      write(72,*) 'LOOKUP_TABLE default'
      do i=1,nel
         write(72,*) u(i,4)
      enddo
      write(72,*) ''

      close(72)

      return
      end
c ---------------------------------------------------------------------
      subroutine read_sorted_grid
c
      include 'MYDATA'

      write(6,*) 'Reading nodel file'
      open(unit=76,file='nodel')
      read(76,*) nn
      do i = 1,nn
         read(76,*) nodel(1,i)
         read(76,*) nodel(2,i)
      enddo
      close(76)
      write(6,*) 'Finished reading nodel file'

      write(6,*) 'Reading elnorm file'
      open(unit=76,file='elnorm')
      read(76,*) nel
      do i = 1,nel
         read(76,*) elnorm(1,i)
         read(76,*) elnorm(2,i)
         read(76,*) elnorm(3,i)
         read(76,*) elnorm(4,i)
         read(76,*) elnorm(5,i)
         read(76,*) elnorm(6,i)
         read(76,*) elnorm(7,i)
         read(76,*) elnorm(8,i)
         read(76,*) elnorm(9,i)
         read(76,*) elnorm(10,i)
         read(76,*) elnorm(11,i)
      enddo
      close(76)
      write(6,*) 'Finished reading elnorm file'

      write(6,*) 'Reading elml file'
      open(unit=76,file='elml')
      read(76,*) nel
      do i = 1,nel
         read(76,*) elml(1,i)
         read(76,*) elml(2,i)
         read(76,*) elml(3,i)
         read(76,*) elml(4,i)
         read(76,*) elml(5,i)
         read(76,*) elml(6,i)
         read(76,*) elml(7,i)
         read(76,*) elml(8,i)
         read(76,*) elml(9,i)
      enddo
      close(76)
      write(6,*) 'Finished reading elml file'

      write(6,*) 'Reading edgl file'
      open(unit=76,file='edgl')
      read(76,*) ned
      do i = 1,ned
         read(76,*) edgl(1,i)
         read(76,*) edgl(2,i)
         read(76,*) edgl(3,i)
         read(76,*) edgl(4,i)
         read(76,*) edgl(5,i)
         read(76,*) edgl(6,i)
      enddo
      close(76)
      write(6,*) 'Finished reading edgl file'

      nmm = neq*nel ! matrix actual size

      return
      end
c ---------------------------------------------------------------------
      subroutine read_user_input_file
c
      include 'MYDATA'

      real*8 mmass

      open(unit=81,file="prj.ini",form="formatted")
      read (81,*) dt
      read (81,*) iostep
      read (81,*) nstepmax
      read (81,*) rgam
      read (81,*) mmass
      read (81,*) k
      read (81,*) time_scheme
      read (81,*) conv_scheme
      do i=1,5 ! dirichlet
         read (81,*) bcvals(1,i),bcvals(2,i),bcvals(3,i),bcvals(4,i)
      enddo
      do i=6,10 ! flux
         read (81,*) bcvals(1,i),bcvals(2,i)
      enddo
      close(81)


      rideal = 8.3144598
      rspec  = rideal/mmass
      rcp    = rspec/(1.-1./rgam)
      rcv    = rcp/rgam


      return
      end
c ---------------------------------------------------------------------
