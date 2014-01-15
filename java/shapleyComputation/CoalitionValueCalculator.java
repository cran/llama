package shapleyComputation;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.Scanner;

public class CoalitionValueCalculator
{	
	
	//*********************************************************************************

	public static double[] computeCoalitionValuesBasedOnMinimumTime( double[][] dataMatrix )
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
				double min = Double.MAX_VALUE;
				for(int k=0; k<coalitionSize; k++) // A loop over all members of current coalition
				{
					if( min > dataMatrix[ j ][ coalition[k]-1 ] )
						min = dataMatrix[ j ][ coalition[k]-1 ] ;
				}
				coalitionValues[i] += min;
			}
		}
		return( coalitionValues );
	}

	public static double[] computeCoalitionValuesBasedOnMinimumTime( String dataFilePathAndName )
	{
		//Read the data from the file and put it in a matrix.
		//Every row represents a problem instance, and every column represents a solver.
		double[][] dataMatrix = readRuntimeDataFormFile( dataFilePathAndName );
		
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
				double min = Double.MAX_VALUE;
				for(int k=0; k<coalitionSize; k++) // A loop over all members of current coalition
				{
					if( min > dataMatrix[ j ][ coalition[k]-1 ] )
						min = dataMatrix[ j ][ coalition[k]-1 ] ;
				}
				coalitionValues[i] += min;
			}
		}
		return( coalitionValues );
	}
	
	//*********************************************************************************
	
	private static double[][] readRuntimeDataFormFile( String dataFilePathAndName )
	{			
		try{
			//Use a buffer reader to quickly scan the file (to determine the number of rows and columns)
			BufferedReader bufferReader = new BufferedReader( new FileReader( dataFilePathAndName ) );
			
			//Extract from the data file the number of solvers
			String line = bufferReader.readLine();
			int numOfSolvers = 0;
			for(int i=0; i<line.length(); i++)
				if(line.charAt(i) == ' ')
					numOfSolvers++;
			
			//Extract from the data file the number of problem instances
			int numOfInstances = 0;
			while( line != null ){
				numOfInstances++;
				line = bufferReader.readLine();				
			}
			bufferReader.close();			
			
			//Allocate memory for the matrix in which we are going to put the data
			double[][] dataMatrix = new double[ numOfInstances ][ numOfSolvers ];
			
			//Read the data from the file, and put them in the data matrix
			bufferReader = new BufferedReader( new FileReader( dataFilePathAndName ) );
			for(int row=0; row<numOfInstances; row++){
				line = bufferReader.readLine();
				Scanner scanner = new Scanner( line );
				scanner.next();
				for(int column=0; column<numOfSolvers; column++)
					dataMatrix[row][column] = new Double(scanner.next());
				scanner.close();
			}
			bufferReader.close();
			return(dataMatrix);
		}
		catch (Exception e){
			System.out.println(e);
			return null;
		}
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
