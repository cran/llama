package shapleyComputation;

import java.util.Arrays;

public class ShapleyComputation
{
	//*********************************************************************************
	
	/**
	 * Given the value of every possible coalition, compute the Shapley value of "agent"
	 *	
	 *	@param agent  The agent whose Shapley value is to be computed. Here, the first agent
	 *	is "1", not "0" 
	 *	
	 *	@param coalitionValues  An array containing the value of every coalition. For example,
	 *	coalitionValue[11] = coalitionValue of coalition [1011], i.e., v( {a_1, a_2, a_4} ).
	 *  coalitionValue[0] is the value of the empty set, which is assumed to be 0.
     *
     *  @param minimize  Whether the "value" of the coalition is to be
     *  minimized.
	 *  
	 *	@return	the Shapley value of "agents" 
	 */
	public static double computeShapleyValueOfOneAgent( int agent, double[] coalitionValues, boolean minimize)	                     
	{
		// Based on the number of values in "coalitionValues", figure out the number of agents
		int numOfAgents = (int) (Math.log10(coalitionValues.length) / Math.log10(2));
		
		// compute the bit that represents the agent. For example, for agent a_4, bitRepresentingAgent = 8 = 1000
		int bitRepresentingAgent = (int) Math.pow(2, agent - 1);
		
		// initialization		
		int numOfCoalitions = coalitionValues.length;
		double shapleyValue = 0;
		
		// Compute the factor in Shapley's formula
		// More specifically, given n agents, and a coalition of size s, the factor is: s!(n-s-1)!/n!
		double[] factor = new double[ numOfAgents ];
		for (int size = 0; size <= numOfAgents - 1; size++ )
			factor[size] = 1.0 / ( Combinations.binomialCoefficient( numOfAgents, size ) * (numOfAgents - size) );

		// iterate through all binary numbers from 0 to 2^n (each number represents one unique coalition)  
		for (int i = 0; i < numOfCoalitions; i++)
		{				
			// if the bit corresponding to the current agent is 1 (i.e., if "agent" is a member of coalition "i")
			if ((bitRepresentingAgent & i) == 0)
			{					
				int coalitionWithoutAgent = i;
				int coalitionWithAgent = i + bitRepresentingAgent;
				int sizeOfCoalitionWithoutAgent = Integer.bitCount(i);
				double marginalContribution;
                if(minimize) { marginalContribution = coalitionValues[ coalitionWithoutAgent ] - coalitionValues[ coalitionWithAgent ]; }
                else { marginalContribution = coalitionValues[ coalitionWithAgent ] - coalitionValues[ coalitionWithoutAgent ]; }
				shapleyValue += factor[ sizeOfCoalitionWithoutAgent ] * marginalContribution;
			}
		}
		return shapleyValue;
	}
	
	//*********************************************************************************
	
	/**
	 * Given the value of every possible coalition, compute the Shapley value of each agent
	 *	
	 *	@param coalitionValues  An array containing the value of every coalition. For example,
	 *	coalitionValue[11] = coalitionValue of coalition [1011], i.e., v( {a_1, a_2, a_4} ).
	 *  coalitionValue[0] is the value of the empty set, which is assumed to be 0.
     *
     *  @param minimize  Whether the "value" of the coalition is to be
     *  minimized.
	 *  
	 *	@return	An array (dentoed shapleyValues) consisting of the Shapley value of each agent.
	 *	For example, shapleyValues[0] is the Shapley value of agent a_1. 
	 */
	public static double[] computeShapleyValues(double[] coalitionValues, boolean minimize)
	{
		// Based on the number of values in "coalitionValues", figure out the number of agents
		int numOfAgents = (int) (Math.log10(coalitionValues.length) / Math.log10(2));
	
		// Compute the Shapley value of each agent
		double[] shapleyValues = new double[ numOfAgents ];
		for (int agent = 1; agent <= numOfAgents; agent++){
			shapleyValues[agent-1] = computeShapleyValueOfOneAgent( agent, coalitionValues, minimize);
		}
		return shapleyValues;
	}
}
