package shapleyComputation;

import java.math.BigInteger;

/**
 * Class that contains general combinatorial functions such as computing the number of possible
 * subsets of a set of a certain size, generating a list of the possible subsets, and so on...
 */
public class Combinations
{
	private static long[][] pascalMatrix = null;
	
	//******************************************************************************************************
	
	/**
	 * This method initializes pascal's Matrix. For more details, see "An Algorithm for 
	 * Distributing Coalitional Value Calculations Among Cooperative Agents" [AIJ, 2007]
	 */
	public static long[][] initPascalMatrix( int numOfLines, int numOfColumns )
	{
		if(( pascalMatrix==null )||( numOfLines > pascalMatrix.length ))
		{
			if( pascalMatrix == null ) //Build the matrix from scratch
			{
				pascalMatrix = new long[numOfLines][numOfColumns];
				for(int i=0; i<numOfLines;   i++) { pascalMatrix[i][0]=1;  }
				for(int j=1; j<numOfColumns; j++) {	pascalMatrix[0][j]=j+1;}

				for(int i=1; i<numOfLines; i++)
					for(int j=1; j<numOfColumns; j++)
						pascalMatrix[i][j]=pascalMatrix[i-1][j]+pascalMatrix[i][j-1];
			}
			else //extend the already-built matrix (only if the previous one was smaller than the current one)
			{
				long[][] prev_pascalMatrix = pascalMatrix;
				int prev_numOfLines   = prev_pascalMatrix.length;
				int prev_numOfColumns = prev_pascalMatrix[0].length; 
				
				pascalMatrix = new long[numOfLines][numOfColumns];
				for(int i=0; i<numOfLines  ; i++) { pascalMatrix[i][0]=1;  }
				for(int j=1; j<numOfColumns; j++) { pascalMatrix[0][j]=j+1;}
				
				for(int i=1; i<prev_numOfLines; i++)
				{
					//Copy the pervious columns of the previous lines
					for(int j=1; j<prev_numOfColumns; j++)
						pascalMatrix[i][j]=prev_pascalMatrix[i][j];
					//Add the new columns to the previous lines
					for(int j=prev_numOfColumns; j<numOfColumns; j++)
						pascalMatrix[i][j]=pascalMatrix[i-1][j]+pascalMatrix[i][j-1];;
				}
				//Build the new lines
				for(int i=prev_numOfLines; i<numOfLines; i++)
					for(int j=1; j<numOfColumns; j++)
						pascalMatrix[i][j]=pascalMatrix[i-1][j]+pascalMatrix[i][j-1];
				
				//Free the previous matrix
				prev_pascalMatrix=null;
			}
		}
		return( pascalMatrix );
	}
	
	//******************************************************************************************************
	
	/**
	 * This function returns x choose y. In other words, it returns:  factorial(x) / factorial(x-y)*factorial(y) 
	 */
	public static long binomialCoefficient(int x, int y)
	{
		if( x==y ) return( 1 ); //Deal with this special case

		//initialize pascal matrix (if it hasn't already been initialized)
		initPascalMatrix( x, x );

		//By this, we have: C^x_y = pascalMatrix[x-y-1,y] (with the exception of the case where n = m)
		return( pascalMatrix[x-y-1][y] );
        /*
        //Alternative method...
		int result = 1;
        if( y > x / 2 ) 
        	y = (int)(x - y);
        for (int i = 1; i <= y; i++)
        	result = result * (x + 1 - i) / i;
        return result; */
	}
	
	//******************************************************************************************************
	
    /**
     * Given a subset in bit format, return its size (i.e. the number of ones it contains)
     */
    public static int getSizeOfSubsetInBitFormat( int subsetInBitFormat, int numOfAgents )
    {
    	return( Integer.bitCount( subsetInBitFormat ) );
    	/*
        int result = 0;
        for (int i = 0; i < numOfAgents; i++)
            if ((subsetInBitFormat & (1<<i)) != 0) //if agent "i+1" is a member of "subsetInBitFormat"
                result++;

        return result;
        */
    }	
	
	//******************************************************************************************************
	
    /**
     * Compute the number of possible "coalition structures", i.e., partitions.
     */
    public static long getNumOfPartitions( int numOfAgents )
    {
    	long numOfPartitions =0;
    	for(int size=1; size<=numOfAgents; size++)
    		numOfPartitions += getNumOfPartitionsOfCertainSize(numOfAgents,size);
    	return( numOfPartitions );
    }
    //The following is an alternative approach that uses "BigInteger"
    public static BigInteger getNumOfPartitions_bitIntegerVersion( int numOfAgents )
    {
    	BigInteger numOfPartitions = BigInteger.valueOf(0);	
    	for(int size=1; size<=numOfAgents; size++)
    		numOfPartitions = numOfPartitions.add(getNumOfPartitionsOfCertainSize_bigIntegerVersion(numOfAgents,size));
    	return( numOfPartitions );
    }
    
    //******************************************************************************************************

    /**
	 * Compute the number of possible "coalition structures" (i.e., partitions) of a particular size.
	 */
	public static long getNumOfPartitionsOfCertainSize( int numOfAgents, int size )
	{
	    if( (size==1)||(size==numOfAgents) )
	    	return(1);
	    else
	    	return(size* getNumOfPartitionsOfCertainSize((int)(numOfAgents-1),size) + getNumOfPartitionsOfCertainSize((int)(numOfAgents-1),(int)(size-1)));
	}	
	//The following is an alternative approach that uses "BigInteger"
	public static BigInteger getNumOfPartitionsOfCertainSize_bigIntegerVersion( int numOfAgents, int size )
	{
	    if( (size==1)||(size==numOfAgents) )
	    	return( BigInteger.valueOf(1) );
	    else
	    {
	    	BigInteger solution = getNumOfPartitionsOfCertainSize_bigIntegerVersion( (int)(numOfAgents-1), size ).multiply( BigInteger.valueOf(size) );
	    	solution = solution.add( getNumOfPartitionsOfCertainSize_bigIntegerVersion( (int)(numOfAgents-1), (int)(size-1) ) );
	    	return( solution );
	    }
	}
	
	//******************************************************************************************************

	/**
	 * Convert a subset (a coalition) from int format to bit format
	 */
	public static int convertSubsetFromByteToBitFormat( int[] subsetInByteFormat )
	{
   		return( convertSubsetFromByteToBitFormat( subsetInByteFormat, subsetInByteFormat.length ) );
	}	
	public static int convertSubsetFromByteToBitFormat( int[] subsetInByteFormat, int subsetSize )
	{
   		int subsetInBitFormat = 0;
   		for(int i=0; i<subsetSize; i++)
   			//Add agent "subsetInByteFormat[i]" to "subsetInBitFormat"
   			subsetInBitFormat += 1 << (subsetInByteFormat[i]-1);

   		return( subsetInBitFormat );
	}
	
	//******************************************************************************************************

	/**
	 * Convert a subset from bit format to int format (e.g. given 4 agents, 0110 becomes {2,3})
	 */
	public static int[] convertSubsetFromBitToByteFormat( int subsetInBitFormat, int numOfAgents, int subsetSize )
	{
		int[] subsetInByteFormat = new int[ subsetSize ];
		int j=0;
		for(int i=0; i<numOfAgents; i++){
			if ((subsetInBitFormat & (1<<i)) != 0){ //if agent "i+1" is a member of "subsetInBitFormat"
				subsetInByteFormat[j]= (int)(i+1);
				j++;
			}
		}
		return( subsetInByteFormat );
	}
	
	//******************************************************************************************************
	
	/**
	 * Convert a subset from bit format to int format (e.g. given 4 agents, 0110 becomes {2,3})
	 */
	public static int[] convertSubsetFromBitToByteFormat( int subsetInBitFormat, int numOfAgents )
	{
		//compute the size of the subset
		int subsetSize = getSizeOfSubsetInBitFormat( subsetInBitFormat, numOfAgents);
		return( convertSubsetFromBitToByteFormat(subsetInBitFormat, numOfAgents, subsetSize) );
	}

	//******************************************************************************************************
	
	/**
	 * Convert a set of subsets (i.e., a "coalition structure" aka a "partition") from bit format to byte format
	 */
	public static int[][] convertSetOfSubsetsFromBitToByteFormat( int[] setOfSubsetsInBitFormat,int numOfAgents)
	{
    	//Initialization
    	int[] sizeOfEachSubset = new int[ setOfSubsetsInBitFormat.length ];    	
        for(int i=setOfSubsetsInBitFormat.length-1; i>=0; i--)
        	sizeOfEachSubset[i] = (int)getSizeOfSubsetInBitFormat( setOfSubsetsInBitFormat[i] , numOfAgents);

        return( convertSetOfSubsetsFromBitToByteFormat(setOfSubsetsInBitFormat,numOfAgents,sizeOfEachSubset));
        }
	
	//******************************************************************************************************
	
	/**
	 * Convert a set of subsets (i.e., a "coalition structure" aka a "partition") from bit to byte format, where the
	 * length of each coalition is provided in the array "lengthOfEachCoalition" (i.e., coalition "CS[i]" is of size "lengthOfEachCoalition[i]"). 
	 */
	public static int[][] convertSetOfSubsetsFromBitToByteFormat(
			int[] setOfSubsetsInBitFormat, int numOfAgents, int[] sizeOfEachSubset )
	{
        int[][] setOfSubsetsInByteFormat = new int[ setOfSubsetsInBitFormat.length ][];
    	for(int i=0; i<setOfSubsetsInBitFormat.length; i++)
    		setOfSubsetsInByteFormat[i] = convertSubsetFromBitToByteFormat( setOfSubsetsInBitFormat[i], numOfAgents); 
    			
    	return(setOfSubsetsInByteFormat);		
	}
	
	//******************************************************************************************************
	
	/**
	 * Convert a set of subset (i.e., a "coalition structure" aka a "partition") from int to bit format 
	 */
	public static int[] convertSetOfSubsetsFromByteToBitFormat( int[][] setOfSubsetsInByteFormat )
	{
    	int[] setOfSubsetsInBitFormat = new int[ setOfSubsetsInByteFormat.length ];
    	for(int i=0; i<setOfSubsetsInByteFormat.length; i++)
    		setOfSubsetsInBitFormat[i] = convertSubsetFromByteToBitFormat( setOfSubsetsInByteFormat[i] );
    	return( setOfSubsetsInBitFormat );
    }
	
	//******************************************************************************************************
	
	/**
	 * - This method returns the list of possible subsets of a set of size = "size".
	 * - Here, a coalition is represented using ints (i.e., each agent is represented using a int)
	 */
	public static int[][] getSubsetsOfGivenSize(int numOfAgents, int size )
	/*
	 * Here, we start by generating the subset: "1,2,...,size" which is the last subset
	 * in the list. Then, from the current subset, we find the subset that is located
	 * before it, and so on until we reach the first subset in the list, which is:
	 * "numOfAgents-(size-1), numOfAgents-(size-2), ..., numOfAgents"
	 */
	{
		int[][] list = new int[ (int)binomialCoefficient(numOfAgents, size) ][size];		
		int index = list.length - 1;
		
		//Generate the subset: "1,2,...,size"
		for(int i=1;i<=size;i++)
			list[index][i-1]=i;
		
		//Generate the remaining subsets
		int maxPossibleValueForFirstAgent = (int)(numOfAgents-size+1);
		while( index>0 )
		{
			for(int i=size-1; i>=0; i--)
			{
				//If the agent at index "i" is smaller than the largest possible agent that can be at index "i"
				if( list[index][i] < maxPossibleValueForFirstAgent+i )
				{					
					index--;
					
					for(int j=0; j<i; j++)
						list[index][j]=list[index+1][j];
					
					list[index][i]=(int)(list[index+1][i]+1);
					
					for(int j=i+1; j<size; j++)
						list[index][j]=(int)(list[index][j-1]+1);
					
					break;
				}
			}
		}
		return(list);
	}
	
	//******************************************************************************************************
	
	/**
	 * - This method returns the list of possible subsets of a set of size = "size".
	 * - Here, a coalition is represented using bits (i.e., each agent is represented using a bit)
	 */
	public static int[] getSubsetsOfGivenSizeInBitFormat(int numOfAgents, int size)
	/*
	 * Here, we start by generating the subset: "1,2,...,size" which is the LAST subset
	 * in the list. Then, from the current subset, we find the subset that is located
	 * BEFORE it, and so on until we reach the first subset in the list, which is:
	 * "numOfAgents-(size-1), numOfAgents-(size-2), ..., numOfAgents"
	 */
	{
		//set "onesBeforeIndex" such that it contains 1,1,1,1... "k" times, and then contains zeros
		final int[] onesBeforeIndex = new int[ numOfAgents+1 ];
		for(int k=numOfAgents; k>0; k--)
			onesBeforeIndex[k] = (1<<k) - 1;

		int[] list=new int[ (int)binomialCoefficient(numOfAgents, size) ];
		int index = list.length - 1;
		
		//Generate the subset: "1,2,...,size"
		list[index]=0;
		for(int i=1;i<=size;i++)
			list[index] += (1<<(i-1)); //add agent "i" to the coalition "list[index]"
		
		//Generate the remaining subsets
		int maxPossibleValueForFirstAgent = (int)(numOfAgents-size+1);
		while( index>0 ) //For every coalition in the list
		{
			//Initializing the index of the agent that we are trying to identify. Here, "size-1"
			int i=size-1; //means that we trying to identify the last agent in the coalition.

			for(int k=numOfAgents; k>0; k--)
			{
				if ((list[index] & (1<<(k-1))) != 0) //If agent "k" is in the coalition "list[index]"
				{
					if( k < maxPossibleValueForFirstAgent+i ) //If agent "k" is smaller than the largest possible agent that can be at index "i"
					{					
						index--;
						
						list[index] = (list[index+1] & onesBeforeIndex[k-1]);

						list[index] += (1<<k); //add agent "k+1" to the coalition "list[index]"

						for(int j=1; j<size-i; j++)
							list[index] += (1<<(k+j)); //add agent "(k+j)+1" to the coalition "list[index]"
						
						i--;
						break;
					}
					i--;
				}
			}
		}
		return(list);
	}
	
	//*********************************************************************************************************
	
	/**
	 * Given a coalition in a list of coalitions that is ordered as in DCVD, this method sets this
	 * coalition to be the coalition located before it. HERE, THE COMBINATION IS GIVEN IN BYTE FORMAT
	 */
	public static void getPreviousSubset( final int numOfAgents, final int size, int[] subset)
	{
		/* maxPossibleValueForFirstAgent: is the maximum value that the first agent in the coalition can have.
		 * For example, if we have coalitions of size 3 out of 9 agents, then, since each subset is ordered
		 * in an ascending order, the first agent cannot be greater than 7, and that is in subset: (7,8,9)
		 */
		final int maxPossibleValueForFirstAgent = (int)(numOfAgents-size+1);
		for(int i=size-1; i>=0; i--) {
			if( subset[i] < maxPossibleValueForFirstAgent+i ) {
					subset[i]++;				
				for(int j=i+1; j<size; j++) {
					subset[j]=subset[j-1]+1;
				}
				break;
			}
		}			
	}
	
	//************************************************************************************************

	/**
	 * Given a coalition in a list of coalitions that is ordered as in DCVD, this method sets this coalition
	 * to be the coalition located before it. HERE, THE COMBINATION IS GIVEN IN BIT FORMAT
	 */
	public static int getPreviousSubsetInBitFormat( final int numOfAgents, final int size, int subset)
	{
		//Initializing the index of the agent that we are trying to identify. Here, "size-1"
		int i=size-1; //means that we trying to identify the last agent in the coalition.

		/* maxPossibleValueForFirstAgent: is the maximum value that the first agent in the coalition can have.
		 * For example, if we have coalitions of size 3 out of 9 agents, then, since each subset is ordered
		 * in an ascending order, the first agent cannot be greater than 7, and that is in subset: (7,8,9)
		 */
		int maxPossibleValueForFirstAgent = (int)(numOfAgents-size+1);
		for(int k=numOfAgents; k>0; k--)
		{
			if ((subset & (1<<(k-1))) != 0) //If agent "k" is in "subset"
			{
				if( k < maxPossibleValueForFirstAgent+i ) //If agent "k" is smaller than the largest possible agent that can be at index "i"
				{
					subset &= (1<<(k-1)) - 1; //copy the part in "subset" that is
					//before "k", and set the remaining part to zeros

					subset += (1<<k); //to replace agent "k" with agent "k+1"

					for(int j=1; j<size-i; j++) //set the remaining agents
						subset += (1<<(k+j)); //add agent "(k+j)+1" to "subset"
					
					i--;
					break;
				}
				i--;
			}
		}
		return( subset );		
	}
	
	//*********************************************************************************************************
	
	/**
	 * Given a coalition in a list of coalitions that is ordered as in DCVD, this method sets this coalition
	 * to be the coalition located after it. HERE, THE COMBINATION IS REPRESENTED IN BYTE FORMAT.
	 */
	public static void getNextSubset( int numOfAgents, int size, int[] subset)
	{
		/* maxPossibleValueForFirstAgent: is the maximum value that the first agent in the coalition can have.
		 * For example, if we have coalitions of size 3 out of 9 agents, then, since each subset is ordered
		 * in an ascending order, the first agent cannot be greater than 7, and that is in subset: (7,8,9)
		 */
		final int maxPossibleValueForFirstAgent = (int)(numOfAgents-size+1);
		for(int i=size-1; i>0; i--) {
			if( subset[i] != subset[i-1]+1 ) {
				subset[i]--;				
				for(int j=i+1; j<size; j++) {
					subset[j]=(int)(maxPossibleValueForFirstAgent+j);
				}
				return;
			}
		}
		//If we reach this instruction, it means that we reached a special case
		subset[0]--;
		for(int j=1; j<size; j++) {
			subset[j]=(int)(maxPossibleValueForFirstAgent+j);
		}		
	}
	
	//******************************************************************************************************
	
	/**
	 * More efficient, but not lexicographic order. I got it from the very end of:
	 * http://graphics.stanford.edu/~seander/bithacks.html#NextBitPermutation
	 */
	public static int getPreviousSubsetInBitFormat2( int numOfAgents, int size, int subset)
	{
		int t = (subset | (subset - 1)) + 1;  
		return( t | ((((t & -t) / (subset & -subset)) >> 1) - 1) );
		
		//A DIFFERENT VERSION, SAME EFFICIENCY IT SEEMS

		//int t = subset | (subset - 1);
		//return( (t + 1) | (((~t & -~t) - 1) >> (Integer.numberOfTrailingZeros(subset) + 1)) );
	}
	
	//******************************************************************************************************
	
	/**
	 * Given a coalition in a list of coalitions that is ordered as in DCVD, this method sets this coalition
	 * to be the coalition located after it. HERE, THE COMBINATION IS REPRESENTED IN BIT FORMAT.
	 */
	public static int getNextSubsetInBitFormat( int numOfAgents, int size, int subset)
	{
		int k2=0;
		
		//Initializing the index of the agent that we are trying to identify. Here, "size-1"
		int i=size-1; //means that we trying to identify the last agent in the coalition.
		
		/* maxPossibleValueForFirstAgent: is the maximum value that the first agent in the coalition can have.
		 * For example, if we have coalitions of size 3 out of 9 agents, then, since each subset is ordered
		 * in an ascending order, the first agent cannot be greater than 7, and that is in subset: (7,8,9)
		 */
		final int maxPossibleValueForFirstAgent = (int)(numOfAgents-size+1);
		for(int k=numOfAgents; k>0; k--)
		{
			if((subset & (1<<(k-1))) != 0) //If agent "k" is in the coalition
			{
				if((subset & (1<<(k-2))) == 0) //If agent "k-1" is not in the coalition
				{
					subset &= (1<<(k-1)) - 1; //copy the part in "subset" that is
					//before "k", and set the remaining part to zeros
					
					subset += (1<<(k-2)); //This way, we replace agent "k" with "k-1"				

					for(int j=i+1; j<size; j++) //set the remaining agents
						subset += (1<<(maxPossibleValueForFirstAgent+j - 1));

					return( subset );
				}
				i--;
				if(i==0)
				{
					//Store the first agent in the coalition
					k2 = k-1;
					while( (subset & (1<<(k2-1))) == 0 ) { //while agent "k2" is not in "subset"
						k2--;
					}
					break;
				}
			}
		}
		//If we reach this instruction, it means that we reached a special case
		subset = (1<<(k2-2)); //Initialize the coalition to contain only agent "k2-1"
		for(int j=1; j<size; j++)
			//Add agent "(maxPossibleValueForFirstAgent+j)" to "subset"
			subset += (1 << (int)( (maxPossibleValueForFirstAgent+j) - 1));
		
		return( subset );
	}

	//******************************************************************************************************
	
	/**
	 * This method generates the coalitions located at the given index (assuming that
	 * the coalitions are order as in DCVC).
	 * 
	 * IMPORTANT EXAMPLE:
	 * We have 10 possible coalitions of size 3 out of 5 agents. Now if index=0, then this
	 * method would return "3,4,5", and if index=9, the method would return "1,2,3" 
	 */
	public static int[] getSubsetAtGivenIndex(int size, int index, int numOfAgents)
	{
		//Initialization
		index++;
		initPascalMatrix( numOfAgents+1, numOfAgents+1 );
		int[] M=new int[size];
		boolean done=false;		
	
		/*1*/ int j=1; int s1=size;
		do
		{
			//Check the values: PascalMatrix[s1,1],PascalMatrix[s1,2],...
			/*2*/ int x=1;  while( pascalMatrix[s1-1][x-1] < index )  x++;
	
			/*3*/ M[j-1]=(int)( (numOfAgents-s1+1)-x+1 );
	
			/*4*/ if( pascalMatrix[s1-1][x-1]==index )
			{
				//Set the rest of the coalition as follows:
				for(int k=j; k<=size-1; k++) M[k]=(int)( M[k-1]+1 );
				done=true;
			}
			else //Otherwise
			{
				j++;  index -=pascalMatrix[s1-1][x-2];  s1--;
			}
		}
		while( done==false );
		return(M);
	}

	//*********************************************************************************************************
	
	/**
	 * Given a coalition C : |C|=s, this method returns the index of C in the list of coalitions of size s
	 * IMPORTANT EXAMPLE:
	 * We have 10 possible coalitions of size 3 out of 5 agents. Now if the coalition is "3,4,5",
	 * then this method would return 0, and if the coalition is "1,2,3", the method returns 9.  
	 */	
	public static int getIndexOfSubset( final int[] subset, final int size, final int numOfAgents )
	{
		long indexOfSubset=0;
		if( size==1 )
			indexOfSubset = numOfAgents-subset[0]+1;
		else
		{
			boolean sequence=true;
			for(int i1=size-1; i1>=1; i1--) {
				if( subset[i1]-subset[i1-1]>1 ) {
					indexOfSubset = pascalMatrix[ size-i1-1 ][ (numOfAgents-size+i1)-subset[i1]+1 ];
					for(int i2=i1-1; i2>=0; i2--) {					
						indexOfSubset += pascalMatrix[ size-i2-1 ][ (numOfAgents-size+i2)-subset[i2] ];
					}
					sequence=false;
					break;
				}
			}
			if( sequence )
				indexOfSubset=pascalMatrix[ size-1 ][ numOfAgents-size-subset[0]+1 ];
		}
		return( ((int)indexOfSubset) - 1 );
	}
}
