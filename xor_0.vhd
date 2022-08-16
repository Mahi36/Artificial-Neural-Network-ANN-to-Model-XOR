

--	Read about sigmoid function ( it appears in popular logistic regression too ).

-- The sigmoid function shape is approximated in the entity "sigmoid_approx" ...

--		Note that the input to sigmoid_approx entity, i.e. Y, is in 2's complement format.
--		When Y is positive, it represents the real/rational value given by integer(Y) / 2**8 
--		When Y is negative, then abs_Y represents the real/rational value given by integer(abs_Y) / 2**8
--		
--		Recall  2's complement representation, to note that the 2's complement is found by complementing each bit
--			and then adding 1 to it.
--		You will find vhdl code statements performing such operation in the architecture of sigmoid_approx
--			provided herewith
------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

package data_types is
    type vector is array (natural range <>) of STD_LOGIC_VECTOR (15 downto 0); 
    function to_vec (slv: std_logic_vector) return vector;
     function to_slv (v: vector) return std_logic_vector;
end data_types;

package body data_types is

    function to_vec (slv: std_logic_vector) return vector is
    variable c : vector (0 to (slv'length/16)-1);
    begin
        for I in c'range loop
            c(I) := slv((I*16)+15 downto (I*16));
        end loop;
        return c;
    end function to_vec;
    
    function to_slv (v: vector) return std_logic_vector is
    variable slv : std_logic_vector ((v'length*16)-1 downto 0);
    begin
        for I in v'range loop
            slv((I*16)+15 downto (I*16)) := v(I);  
        end loop;
        return slv;
    end function to_slv;
end data_types; 



----------------------------------------------------------------------------------
-- Module Name: sigmoid_approx - Behavioral
-- Description: Implements sigmoid function mathematically.
----------------------------------------------------------------------------------


--library IEEE;
--use IEEE.STD_LOGIC_1164.ALL;
--use IEEE.NUMERIC_STD.ALL;
--
--entity sigmoid_approx is
--    Port ( Y : in STD_LOGIC_VECTOR (15 downto 0);
--           O : out STD_LOGIC_VECTOR (15 downto 0);
--           clk: in STD_LOGIC );
--end sigmoid_approx;
--
--
--architecture behav of sigmoid_approx is
--    signal abs_Y : std_logic_vector( 15 downto 0 ) ;
--    signal O_sig : std_logic_vector( 15 downto 0 ) ;
--begin
--	abs_Y <= Y when Y(15)='0' else std_logic_vector ( unsigned ( Y xor X"FFFF" ) + 1 ) ;	
--	
--	process ( clk ) begin
--		if ( rising_edge ( clk ) ) then
--			if ( Y(15) = '0' ) then 
--				O_sig <= X"00" & "1" & std_logic_vector( Y(8 downto 2) ) ; 
--				if ( unsigned(Y(15 downto 8)) > 2 ) then 
--					O_sig <= X"00FF" ;
--				end if ;
--			elsif ( Y(15) = '1' ) then 
--				O_sig <= X"00" & "0" & std_logic_vector( abs_Y(8 downto 2) ) ; 
--				if ( unsigned(abs_Y(15 downto 8)) > 2 ) then 
--					O_sig <= X"0000" ;
--				end if ;
--			end if ;
--		end if ;
--	end process ;
--
--	O <= O_sig ;
--	     
--end behav;

----------------------------------------------------------------------------------
-- Module Name: sigmoid_approx - Behavioral
-- Description: Implements ROM based LUT to store the values obatined in sigmoid function.
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity sigmoid_approx  is 
   Generic (n: integer:= 16);
   port(Y: in std_logic_vector(n-1 downto 0);
	     O : out STD_LOGIC_VECTOR (n-1 downto 0);clk: in STD_LOGIC);
end sigmoid_approx ;

architecture behav of sigmoid_approx  is

 signal Address,Output:std_logic_vector( 7 downto 0 ) ;
 signal abs_Y: std_logic_vector( 15 downto 0 ) ;
 signal O_sig : std_logic_vector( 15 downto 0 ) ;
 
begin
	abs_Y <= Y when Y(15)='0' else std_logic_vector ( unsigned ( Y xor X"FFFF" ) + 1 ) ;
	with Y(15) select
   Address(7 downto 0) <= '1'& Y(8 downto 2) when '0',
							'0'& abs_Y(8 downto 2) when others;
p1:process(Address)
begin
Output <=  Address;
end process p1;
p2:	process ( clk ) begin
		if ( rising_edge ( clk ) ) then
			
				O_sig <= X"00" &  std_logic_vector(Output(7 downto 0)) ; 
				if ( Y(15) = '0' ) then 
				
				   if ( unsigned(Y(15 downto 8)) > 2 ) then 
					O_sig <= X"00FF" ;
				   end if ;
				elsif ( Y(15) = '1' ) then 
				   if ( unsigned(abs_Y(15 downto 8)) > 2 ) then 
					O_sig <= X"0000" ;
				   end if ;
				end if;
		end if ;
	end process p2;
	O <= O_sig ;
end behav;

----------------------------------------------------------------------------------
-- Module Name: Neuron - Behavioral
-- Description: Implements a neuron prepared to be connected into a network using an aproximation of the sigmoid
--  function based on a ROM and using Q15.16 signed codification.
----------------------------------------------------------------------------------

library IEEE; use IEEE.STD_LOGIC_1164.ALL; use IEEE.NUMERIC_STD.ALL;
library work; use work.data_types.all;
entity Neuron is
    generic ( n : integer := 2 );
    Port ( slv_Xin, slv_Win : in STD_LOGIC_VECTOR ((n*16)+15 downto 0); --Values
           clk : in STD_LOGIC;
           O : out STD_LOGIC_VECTOR (15 downto 0)  );
end Neuron;
architecture Behavioral of Neuron is
    component sigmoid_approx
        port ( Y : in STD_LOGIC_VECTOR (15 downto 0);
               O : out STD_LOGIC_VECTOR (15 downto 0);   clk: in STD_LOGIC );
    end component;
    signal sum, sum1 : signed(15 downto 0) := x"0000";
    signal Y : STD_LOGIC_VECTOR (15 downto 0); 
    signal Xin, Win, Prod : vector (0 to n) := (others => x"0000"); 
    signal d : STD_LOGIC_VECTOR ((n*16)+15 downto 0); 
	for SIG : sigmoid_approx use entity work.sigmoid_approx ( behav ) ;
begin
    SIG : sigmoid_approx port map (Y => Y, O => O, clk => clk);
	sum1 <=  signed(Prod(2)) + signed(Prod(1)); sum <=  sum1 + signed(Prod(0)) ;
    Xin <= to_vec(slv_Xin);    Win <= to_vec(slv_Win);    d <= to_slv(Prod);
    process (Xin, Win,sum) begin 
        L1: for I in 0 to n loop
            Prod(I) <= to_stdlogicvector(
				to_bitvector(std_logic_vector(signed(Xin(I)) * signed(Win(I)))) sra 8)(15 downto 0);
        end loop L1;
    end process;
    process (clk) begin
        if rising_edge(clk) then
           Y <= std_logic_vector(sum); 
        end if;
    end process;
end Behavioral;


---------------------------------------------------------------------------------- 
-- Module Name: XorNN
-- Description: Implements an ANN fr XOR by connecting up Neuron 
----------------------------------------------------------------------------------

library ieee; use ieee.std_logic_1164.all; use ieee.numeric_std.all ;

entity XorNN is
    	Port ( A,B : in STD_LOGIC_VECTOR (15 downto 0); 
           clk : in STD_LOGIC;
           O : out STD_LOGIC_VECTOR (15 downto 0)  );
end entity ;
architecture rch1 of XorNN is 
	component Neuron is
    	generic ( n : integer := 0 );
    	Port ( slv_Xin, slv_Win : in STD_LOGIC_VECTOR ((n*16)+15 downto 0); --Value 
        	clk : in STD_LOGIC;
			O : out STD_LOGIC_VECTOR (15 downto 0)  );
	end component;
    constant n_inp_hidden_layer, n_inp_out_layer : integer := 2 ;
    constant wt_hidden_layer_node_1
    	: STD_LOGIC_VECTOR ((n_inp_hidden_layer*16)+15 downto 0) 
        := std_logic_vector( to_signed( (-6)*1 * 2**8 , 16 ) )
        	& std_logic_vector( to_signed( 6*1 * 2**8 , 16 ) ) 
            & std_logic_vector( to_signed( (-2)*1 * 2**8 , 16 ) ) ;
    constant wt_hidden_layer_node_0 
    	: STD_LOGIC_VECTOR ((n_inp_hidden_layer*16)+15 downto 0) 
        := std_logic_vector( to_signed( 6*1 * 2**8 , 16 ) )
        	& std_logic_vector( to_signed( (-6)*1 * 2**8 , 16 ) ) 
            & std_logic_vector( to_signed( (-2)*1 * 2**8 , 16 ) ) ;
    constant wt_output_layer_node_0 
    	: STD_LOGIC_VECTOR ((n_inp_out_layer*16)+15 downto 0) 
        := std_logic_vector( to_signed( 4*1 * 2**8 , 16 ) )
        	& std_logic_vector( to_signed( 4*1 * 2**8 , 16 ) ) 
            & std_logic_vector( to_signed( (-3)*1 * 2**8 , 16 ) ) ;          
	constant one_fixed_8_8 : std_logic_vector( 15 downto 0 ) 
    		:= std_logic_vector( to_signed( 1 * 2**8 , 16 ) ) ;
    signal out_h1 , out_h0 : std_logic_vector( 15 downto 0 ) 
    		:= ( others => '0' ) ;
	signal sig1, sig2, sig3 : STD_LOGIC_VECTOR ((n_inp_out_layer*16)+15 downto 0) ;
	signal O_sig : std_logic_vector( 15 downto 0 )  ;
begin
	O <= O_sig ;
	sig1 <= ((A & B) & one_fixed_8_8) ;
	sig2 <= ((A & B) & one_fixed_8_8) ;
	sig3 <= ((out_h0 & out_h1) & one_fixed_8_8) ;
	
	hidden_layer_node_1 : Neuron 
    	generic map ( n => n_inp_hidden_layer ) 
        port map (  sig1  ,    	wt_hidden_layer_node_1 ,  clk , out_h1 ) ;
	hidden_layer_node_0 : Neuron 
    	generic map ( n => n_inp_hidden_layer ) 
        port map ( sig2 , wt_hidden_layer_node_0 ,  clk , out_h0 ) ;
	output_layer_node_0 : Neuron 
    	generic map ( n => n_inp_out_layer ) 
        port map ( sig3, wt_output_layer_node_0 ,  clk , O_sig ) ;
                
end rch1 ;

----------------------------------------------------------------------------------
-- Module Name: Testbench  XorNN_test
-- Description: Implements an ANN fr XOR by connecting up Neuron 
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
library work;
use work.data_types.all;

entity XorNN_test is end entity ;
architecture stim of XorNN_test is
	component XorNN is Port ( A,B : in STD_LOGIC_VECTOR (15 downto 0);
           clk : in STD_LOGIC; O : out STD_LOGIC_VECTOR (15 downto 0)  );
	end component ;
    signal A,B :  STD_LOGIC_VECTOR (15 downto 0); 
	signal clk : STD_LOGIC := '0' ;
    signal out_xor_nn_fixed_point_8_8 :  STD_LOGIC_VECTOR (15 downto 0 ) ;   
    function from_sl_to_fixed_8_8 ( inp : std_logic )  return std_logic_vector is
	begin
    	if ( inp = '1' ) then return std_logic_vector( to_signed( 1 * 2**8 , 16 ) ) ;
		else   return std_logic_vector( to_signed( 0 * 2**8 , 16 ) ) ;
		end if ;
    end function ;
begin
	dut_XorNN : XorNN port map ( A,B, clk , out_xor_nn_fixed_point_8_8 ) ;
    process 
    	variable i_2bit : std_logic_vector( 1 downto 0 ) ; 
    begin
        A <= std_logic_vector( to_signed( 0 * 2**8 , 16 ) ) ;
		B <= std_logic_vector( to_signed( 1 , 16 ) ) ;
		wait for 0 ns ;
       for i in 0 to 3 loop
        	i_2bit := std_logic_vector( to_unsigned( i,2 ) ) ;
	        A <= from_sl_to_fixed_8_8( i_2bit(1) )  ;
	        B <= from_sl_to_fixed_8_8( i_2bit(0) )  ;
        	wait for 8*2*10 ns ;
        end loop ;
		wait ;
	end process ;
    process begin
    	clk <= '0' ;
		for i in 0 to 40 loop
        	wait for 10 ns ;  clk <= '1' ; wait for 10 ns ;  clk <= '0' ;
        end loop ;
        wait ;
    end process ;
end stim ;