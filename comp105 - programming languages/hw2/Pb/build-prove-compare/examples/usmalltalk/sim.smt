;;;usm.nw:4650
(val ActiveSimulation nil)
;;;usm.nw:4761
(class Simulation Object
    [now eventQueue]
    (method time-now () now)
    
;;;usm.nw:4810
(method startUp ()
    (set now 0)
    (set eventQueue (new PriorityQueue))
    (ifFalse: (isNil ActiveSimulation)
         {(error: self 'multiple-simulations-active-at-once)})
    (set ActiveSimulation self)
    self)
;;;usm.nw:4820
(method finishUp ()
    (set ActiveSimulation nil)
    self)
;;;usm.nw:4826
(method proceed () [locals event]
   (set event (deleteMin eventQueue))
   (set now (key event))
   (takeAction (value event)))
;;;usm.nw:4843
(method runUntil: (timelimit)
    (startUp self)
    (whileTrue: {(& (not (isEmpty eventQueue)) (<= now timelimit))}
       {(proceed self)})
    (finishUp self)
    self)
;;;usm.nw:4863
(method enter: (anObject) nil)
(method exit:  (anObject) nil)
;;;usm.nw:4875
(method scheduleEvent:at: (anEvent aTime)
    (at:put: eventQueue aTime anEvent))
;;;usm.nw:4883
(method scheduleEvent:after: (anEvent aTimeInterval)
    (scheduleEvent:at: self anEvent (+ now aTimeInterval)))
;;;usm.nw:4917
(method scheduleRecurringEvents:using: (eventFactory timeStream)
    (scheduleNextEvent (new:atNextTimeFrom: RecurringEvents eventFactory timeStream)))
;;;usm.nw:4765
)
;;;usm.nw:4943
(class RecurringEvents Object
    ; represents a stream of recurring events, each created from
    ; 'factory' and occurring at 'times'
    [factory times]
    (method scheduleNextEvent ()
        (scheduleEvent:after: ActiveSimulation self (next times)))
    (method takeAction ()
        (new factory)
        (scheduleNextEvent self))
    (class-method new:atNextTimeFrom: (eventFactory timeStream)
        (init:with: (new super) eventFactory timeStream))
    (method init:with: (f s) ; private
        (set factory f)
        (set times s)
        self)
)
;;;usm.nw:5030
(class Lab Object
    [robot1free robot2free]
    (class-method new () (initLab (new super)))
    (method initLab () ; private
        (set robot1free true)
        (set robot2free true)
        self)
    (method hasARobot? () (| robot1free robot2free))
    (method takeARobot ()
         (if robot1free
              {(set robot1free false) 1}
              {(set robot2free false) 2}))
    (method releaseRobot: (t)
         (if (= t 1) {(set robot1free true)} {(set robot2free true)}))
)
;;;usm.nw:5055
(class RobotLabSimulation Simulation
    [time-limit          ; time limit for using one robot
     lab                 ; current state of the lab
     robot-queue         ; the line of students waiting for a robot
     students-entered    ; the number of students who have entered the lab
     students-exited     ; the number of students who have finished and left
     timeWaiting         ; total time spent waiting in line by students
                         ; who have finished
     student-factory     ; class used to create a new student when one enters
     interarrival-times  ; stream of times between student entries
    ]
    
;;;usm.nw:5090
(class-method withLimit:student:arrivals: (t s as) 
    (init-t:s:as: (new super) t s as))
(method init-t:s:as: (t s as) ; private method
    (set time-limit         t)
    (set student-factory    s)
    (set interarrival-times as)
    self)
;;;usm.nw:5105
(method startUp ()
    (set lab              (new Lab))
    (set students-entered 0)
    (set students-exited  0)
    (set timeWaiting      0)
    (set robot-queue      (new Queue))
    (startUp super)
    (scheduleRecurringEvents:using: self student-factory interarrival-times)
    self)
;;;usm.nw:5119
(class-method new () (error: self 'robot-lab-simulation-needs-arguments))
;;;usm.nw:5129
(method finishUp ()
    (print 'Num-finished=) (print students-exited)
    (printcomma self)
    (print 'left-waiting=) (print (size robot-queue))
    (printcomma self)
    (print 'total-time-waiting=) (print timeWaiting)
    (printcomma self)
    (print 'average-wait=) (println (div: timeWaiting students-exited))
    (finishUp super))
(method printcomma () ; private
    (print ',) (print space))
;;;usm.nw:5143
(method enter: (aStudent)
    (set students-entered (+ 1 students-entered)))
(method exit: (aStudent)
    (set students-exited  (+ 1 students-exited))
    (set timeWaiting      (+ timeWaiting (timeWaiting aStudent))))
;;;usm.nw:5159
(method requestRobotFor: (aStudent)
     (if (hasARobot? lab)
          {(beGrantedRobot: aStudent (takeARobot lab))}
          {(addLast: robot-queue aStudent)}))

(method releaseRobot: (aRobot)
    (releaseRobot: lab aRobot)
    (ifFalse: (isEmpty robot-queue)
       {(beGrantedRobot: (removeFirst robot-queue) (takeARobot lab))}))
;;;usm.nw:5199
(method time-limit       () time-limit)
(method students-entered () students-entered)
;;;usm.nw:5067
)
;;;usm.nw:5185
(class Queue List
    []
    (method species     () Queue)
    (method printName   () (print 'Queue))
)
;;;usm.nw:5317
(class Student Object
    [number          ; uniquely identifies this student
     status          ; 'awaiting-robot, 'finished, or a robot number
     timeNeeded      ; total work time this student needs
     timeStillNeeded ; time remaining for this student
     entryTime       ; time at which this student enters the simulation
     exitTime        ; time at which this student exits the simulation
    ]
    (method print () (print '<Student) (print space) (print number) (print '>))
    
;;;usm.nw:5369
(method timeWaiting ()
    (- exitTime (+ entryTime timeNeeded)))
;;;usm.nw:5382
(method timeNeeded () (subclassResponsibility self))
(class-method new () (init (new super)))
(method init () ; private
  (set number          (+ 1 (students-entered ActiveSimulation)))
  (set status          'awaiting-robot)
  (set timeNeeded      (timeNeeded self))
  (set timeStillNeeded timeNeeded)
  (set entryTime       (time-now ActiveSimulation))
  (enter: ActiveSimulation self)
  (requestRobotFor: ActiveSimulation self)
  self)
;;;usm.nw:5407
(method takeAction ()
   (if (= status 'awaiting-robot)
      {(requestRobotFor: ActiveSimulation self)}
      {(relinquishRobot self)}))
;;;usm.nw:5443
(method relinquishRobot ()
     (releaseRobot: ActiveSimulation status)
     (if (needsRobot? self)
          {(set status 'awaiting-robot)
           (requestRobotFor: ActiveSimulation self)}
          {(set status   'finished)
           (set exitTime  (time-now ActiveSimulation))
           (exit: ActiveSimulation self)}))
;;;usm.nw:5454
(method needsRobot? () (> timeStillNeeded 0))
;;;usm.nw:5467
(method beGrantedRobot: (aRobot) [locals time-to-use]
     (set time-to-use (min: timeStillNeeded (time-limit ActiveSimulation)))
     (set timeStillNeeded (- timeStillNeeded time-to-use))
     (set status aRobot)
     (scheduleEvent:after: ActiveSimulation self time-to-use))
;;;usm.nw:5327
)
;;;usm.nw:5493
(class Student120 Student ; a student needing 120 minutes of robot time
    []
    (method timeNeeded () 120)
)
;;;usm.nw:5512
(class TwentyAtZero Object ; Twenty arrivals at time zero
    [num-arrived]
    (class-method new () (init (new super)))
    (method init () (set num-arrived 0) self)
    (method next ()
         (if (= num-arrived 20)
             {99999}
             {(set num-arrived (+ 1 num-arrived))
              0}))
)
;;;usm.nw:5570
(val last-student-needed 30) ; time needed by last created AlternatingStudent
(class AlternatingStudent Student
    []
    (method timeNeeded ()
         (set last-student-needed (- 150 last-student-needed))
         last-student-needed)
)
;;;usm.nw:5593
(class EveryNMinutes Object
    [interval]
    (class-method new: (n) (init: (new super) n))
    (method init: (n) (set interval n) self)
    (method next () interval)
)
