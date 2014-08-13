package shapleyComputation;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.Scanner;

public class CoalitionValueCalculator
{	
	
	//*********************************************************************************

	public static double[] computeCoalitionValues( double[][] dataMatrix, boolean minimize)
	{
		//initialization
		int numOfProblemInstances = dataMatrix.length;
		int numOfAgents = dataMatrix[0].length;
		int numOfCoalitions = (int)Math.pow(2,numOfAgents);
		double[] coalitionValues = new double[ numOfCoalitions ];
		
		for(int i=1; i < numOfCoalitions; i++ ) // A loop over all coalitions
		{
			//the number "i" represents the coalition in bit format, i.e., every agent is represented as a bit in "i".
			//We convert this into a representation where every agent is represented as a number in an array of integers
			int[] coalition = Combinations.convertSubsetFromBitToByteFormat( i, numOfAgents );
			
			//Initialization
			int coalitionSize = coalition.length;			
			coalitionValues[i] = 0;

			for(int j=0; j<numOfProblemInstances; j++) // A loop over all problem instances
			{
                double opt;
                if(minimize) { opt = Double.MAX_VALUE; }
                else { opt = Double.MIN_VALUE; }
				for(int k=0; k<coalitionSize; k++) // A loop over all members of current coalition
				{
					if(minimize && opt > dataMatrix[ j ][ coalition[k]-1 ]) {
						opt = dataMatrix[ j ][ coalition[k]-1 ] ;
                    } else if(!minimize && opt < dataMatrix[ j ][ coalition[k]-1 ]) {
						opt = dataMatrix[ j ][ coalition[k]-1 ] ;
                    }
				}
				coalitionValues[i] += opt;
			}
		}
		return( coalitionValues );
	}

	//*********************************************************************************

	/**
	 * Compute the maximum value of a singleton coalition, and deduct that from the value of every non-empty coalition
	 */
	public static void deductFromNonEmptyCoalitionsTheMaxSingletonValue( double[] coalitionValues )
	{
		// Based on the number of values in "coalitionValues", figure out the number of agents
		int numOfAgents = (int) (Math.log10(coalitionValues.length) / Math.log10(2));
		
		double max = Double.MIN_NORMAL;
		for(int i=0; i<numOfAgents; i++){
			if( max < coalitionValues[(1 << i)] )
				max = coalitionValues[(1 << i)] ;
		}
		for(int i=1 /*not 0*/; i<coalitionValues.length; i++){
			coalitionValues[i] -= max;
		}
	}
}
