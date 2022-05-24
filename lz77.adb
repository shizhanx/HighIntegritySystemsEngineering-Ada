with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

-- Submission authored by:
-- Shizhan Xu 771900 Kevin Jin Po Yong 901015

-- This file requires Proof Level to be set to: 1

package body LZ77 with SPARK_Mode is

   function Length_Acc(Input : in Token_Array) return Partial_Length is
      Result : Partial_Length (Input'Range) := (others => Zero);
   begin

      for Index in Input'Range loop
         -- Note this loop invariant needs "Proof level = 1" to prove it
         pragma Loop_Invariant
           ((for all I in Input'First .. Index-1 =>
              Result(I) =
            (if I = Input'First then Zero else Result(I - 1)) +
              To_Big_Integer(Input(I).Length) + To_Big_Integer(One)) and then
              (for all I in Input'First .. Index-1 =>
                    (for all J in Input'First..I-1 =>
               Result(I) > Result(J))));
         Result(Index) :=
           (if Index = Input'First then Zero else Result(Index - 1)) +
             To_Big_Integer(Input(Index).Length) + To_Big_Integer(One);
      end loop;
      return Result;
   end Length_Acc;

   procedure Put(T: in Token) is
   begin
      Put("Offset: "); Put(T.Offset); New_Line;
      Put("Length: "); Put(T.Length); New_Line;
      Put("Next_C: "); Put(T.Next_C); New_Line;
   end Put;

   -- Task 3.1: 
   -- The side effects of this procedure is determined, as we only pass
   -- Input as in, it will never get changed here. Likewise, this procedure
   -- is guaranteed to work regardless of what Output_Length and Error are
   -- initially. Also no null-pointer stuff here for we don't use pointers.
   -- On the other hand, Ada provides guarantee for run-time errors like 
   -- those overflow problems. If I could run this program without warnings
   -- from the compiler, I can confirm that this piece of code won't harm
   -- my overall system security. This is impossible for other languages like C.
   -- Task 3.2:
   --
   procedure Decode(Input : in Token_Array; Output : in out Byte_Array;
                    Output_Length : out Natural; Error : out Boolean)
   is
   begin
      -- IMPLEMENT THIS            
      Output_Length := 0;
      Error := False;
      -- Output_Length always points to the last char's position
      -- Loop all tokens and insert chars to the Output_Length position
      for Index in Input'Range loop 
         -- For each token loop Length times to put the previous offset char
         for TokenIndex in 1 .. Input(Index).Length loop
            if Output_Length = Output'Last or 
              Output_Length - Input(Index).Offset + 1 < Output'First
            then 
               Error := True;
               exit;
            end if;
            Output_Length := Output_Length + 1;
            Output(Output_Length) := Output(Output_Length - Input(Index).Offset);
         end loop;
         -- Finally add the last char of that token.
         if Output_Length = Output'Last then 
            Error := True;
            exit;
         end if;
         Output_Length := Output_Length + 1;
         Output(Output_Length) := Input(Index).Next_C;
      end loop;
      if Error = True then Output_Length := 0; end if;
   end Decode;
   
   function Is_Valid(Input : in Token_Array) return Boolean is
      TotalLength: Integer := 0;
   begin
      -- IMPLEMENT THIS      
      for Index in Input'Range loop
         pragma Loop_Invariant (Integer'Last - 1 - Input(Index).Length < TotalLength);
         if (Input(Index).Offset > TotalLength) 
         then return False;
         end if;
         TotalLength := TotalLength + Input(Index).Length + 1;
      end loop;
      return True;
   end Is_Valid;
   
   procedure Decode_Fast(Input : in Token_Array; Output : in out Byte_Array;
                         Output_Length : out Natural)
   is
   begin
      -- IMPLEMENT THIS            
      Output_Length := 0;
      -- Output_Length always points to the last char's position
      -- Loop all tokens and insert chars to the Output_Length position
      for Index in Input'Range loop 
         -- For each token loop Length times to put the previous offset char
         for TokenIndex in 1 .. Input(Index).Length loop
            Output_Length := Output_Length + 1;
            Output(Output_Length) := Output(Output_Length - Input(Index).Offset);
         end loop;
         -- Finally add the last char of that token.
         Output_Length := Output_Length + 1;
         Output(Output_Length) := Input(Index).Next_C;
      end loop;
   end Decode_Fast;

end LZ77;
