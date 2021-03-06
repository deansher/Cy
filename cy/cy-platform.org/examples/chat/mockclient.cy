public module cy-platform.org/examples/chat:mockclient

export (NodeMockClientSystem)

type NodeMockClientSystem = component
  has 1 console :: LineIo
  has 1 stdio :: NodeStdio
  has 1 mockClient :: MockClient

  -- The parameter order for wire doesn't matter.  The runtime will look for a typesafe set
  -- of connections between inputs and outputs and complain if there is not exactly one.
  -- Note that a reply can serve as an output, which is what happens with mockClient.commands.
  
  wire mockClient.commands console.lineIo

  -- Alternatively, we can specify the wiring at the level of individual inputs and outputs.
  -- The parameter order still doesn't matter. 

  wire console.rawIo.fromStdin stdio.io.fromStdin
  wire console.rawIo.toStdout stdio.io.toStdout

  -- When wiring at the level of individual inputs and outputs, there is also a way to 
  -- indicate that a given input's reply should be sent to a particular input.  If done this way, the
  -- above example of mockClient.commands would look like this:
  --
  -- wire mockClient.commands.command (console.lineIo.fromStdin, console.lineIo.toStdout)

type MockClient = component

  var history := ChatHistory

  port commands
    input command :: String >> String handles
      c ->
        -- We pointlessly delegate handling of this command to the processCommand action, to show how
        -- actions are declared and used.  The body of processCommand could instead appear here.
      
        -- Neither the `answer` variable nor its type declaration are required, but they show what is going on.
        -- The processCommand action will occur asynchronously to this action.
        
        answer :: Future String
        answer = send processCommand c

        after answer handles
          response -> response
          exception UnknownCommand msg -> msg

    -- An action is a message from a component to itself.  Otherwise, it is identical to an input.

    action processCommand :: String >> String handles
      c ->
      
        -- A function can't throw an exception. By convention, it indicates errors by returning Either.
        -- The reasoning behind this is that a function has no environment, and an exception is a poor
        -- way to signal invalid parameters.
        
        answer = case parseVerb c of

          -- An action handler can throw an exception, which causes an immediate exit from the action 
          -- handler plus an out-of-band signalling mechanism on the reply.
        
          Just "help" ->
              [stringLines
               | ":history" shows chat history.
               | ":clear" clears chat history.
               | Anything else that begins with a colon is an error.
               |
               | "!xyz" repeats the most recent command that started with "xyz".
               | "?xyz" repeats the most recent command that contained "xyz".
               |      (These can include punctuation such as ":".)
               |
               | Anything else is a message to be sent into the chat.
               |]
          Just "history" ->
            history.prettyPrint
          Just "clear" ->
            history .= clear
            "History cleared."
          Just _ ->
            throw UnknownCommand { message := "Unknown command: " ++ c }
          Nothing ->
            if c.startsWith "!" then
              processCommand history.chFindMostRecentMatching (_.startsWith response[1..])
            else if c.startsWith "?" then
              processCommand history.chFindMostRecentMatching (_.contains response[1..])
            else
              processCommand c
              
        history .= add $ Interaction c answer
        answer

-- UnknownCommand has a single public field called "message".  Since this field has an
-- initial value, the automatically generated constructor UnknownCommand takes no
-- arguments.  However, it is also easy to create an UnknownCommand with a specific message:
-- 
--   UnknownCommand { message := c }
--   
-- The above expression is not a special constructor syntax.  Rather, it is an invocation
-- of the automatically generated constructor, followed by the normal syntax for copying
-- an object while modifying some of its fields.

type UnknownCommand = object
  message := "unknown command"

-- Since ChatHistory has a private field with no initial value, it does not get an
-- automatically generated constructor.

type ChatHistory = object
  -- It would make more sense in this case to give elements an initial value, but instead
  -- we leave it blank to explore Cy's constructor syntax.
  private elements :: [HistoryElement]

  -- A constructor is invoked like a regular function. Internally, it is like a method, except
  -- that it cannot access fields until they are initialized, and it is required to initialize
  -- every field that is not declared with an initial value.  A constructor can be named like
  -- an ordinary function.  If an object class does not get an automatically generated
  -- constructor (as happens when it declares private fields lacking initial values), then
  -- it is also permissible to declare a constructor whose name is the name of the object.
  constructors
    ChatHistory =
      elements := []
      
  methods
    -- This method's full type is ChatHistory -> Int.
    private maxLength = 10

    add :: HistoryElement -> ChatHistory
    add e =
      if length elements >= maxLength then -- we could equivalently write elements.length
        elements .= tail -- shorthand for elements := elements.tail
      elements += e

    -- Returns a copy of this ChatHistory with elements set back to the empty list.
    clear :: ChatHistory
    clear =
      elements := []

    -- Intentionally showing how the method invocation syntax for `map` is different from OO languages.
    prettyPrint = join "\n\n" $ prettyPrint.map elements

    findMostRecentSatisfying :: (HistoryElement -> Boolean) -> Boolean
    findMostRecentSatisfying pred = history.reverse.find pred

{- An interface is equivalent to a Haskell type class.  An interface is specified as a set of top-level
   functions that must each have a type that mentions the target type (`a` in the example below).
 -}

interface PrettyPrintable a where
  methods
    prettyPrint :: a -> String

-- An interface implementation is always declared externally from the definition of the object.
-- If a method of the object has the same name and signature as a method of the interface,
-- then there is no need (and indeed no option) to declare an implementation of that interface method.

implementation PrettyPrintable ChatHistory

-- There is quite a bit of silliness in the HistoryElement class to explore the boundaries
-- of Cy's object model.

type HistoryElement = alternatives
    Interaction
      command :: String
      response :: String
      methods
        -- If a method is defined in any alternative, it must be defined with the same
        -- signature in every alternative.
        formatAsString = command ++ "=>" ++ response
  | SystemEvent
      description :: String
      methods
        formatAsString = description ++ "!"
        
  having
    private formatted = formatAsString
    
    constructors
      parseHistoryElementString :: String -> HistoryElement
      parseHistoryElementString s =
        case s.findSublist "=>" of
          Just i - > (c, rest) = s.splitAt i
                     r = rest.drop "=>".length
                     Interaction c r
          Nothing -> d = if s.endsWith "!"
                         then s.take (s.length - 1)
                         else s
                     SystemEvent d

    methods
      asString = formatted

      size = case self of
        Interaction c r -> c.length + r.length
        SystemEvent d -> d.length
        

implementation PrettyPrintable HistoryElement where
  methods 
    prettyPrint = case self of 
      Interaction c r -> "Command:  " ++ c ++ "\n" ++ "Response: " ++ r
      SystemEvent d ->   "System Event: " ++ d

