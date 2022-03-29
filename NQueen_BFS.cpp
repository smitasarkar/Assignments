#include <iostream>
#include <stack>
#include <queue>
#include <utility>
#define N 8
unsigned int board[N][N]; // D.S. for the board.
unsigned int solutions=0,cnt=1;

typedef std::pair<int, int> position;

using namespace std;

/**
 * Method to print the solution board.
 */

/*int check_1s()
{
	int total = 0;
	for(unsigned int i=0;i<N;i++)
	{
		for(unsigned int j=0;j<N;j++)
			total = total + board[i][j];
	}
	return total;
}*/
void print_board()
{
	std::cout << "--------------------\n";
	for(unsigned int i=0;i<N;i++)
	{
		for(unsigned int j=0;j<N;j++)
			std::cout << board[i][j] << " ";
		std::cout << std::endl;
	}
}
/**
 * Method that checks if the current action keeps the board safe,
 * that is non-attacking.
 */
unsigned int isSafe(unsigned int r, unsigned int c)
{
	//vertical safe check
	for(unsigned int j=0;j<N;j++)
	{
		if(j!=c && board[r][j]==1) return 0;
	}
	// horizontal safety check
	for(unsigned int i=0;i<N;i++)
	{
		if(i!=r && board[i][c]==1) return 0;
	}
	// diagonal check
	for(int i=r-1,j=c-1;i>=0 && j>=0;i--,j--)
		if(board[i][j]==1) return 0;
	for(int i=r+1,j=c+1;i<N && j<N;i++,j++)
			if(board[i][j]==1) return 0;
	for(int i=r-1,j=c+1;i>=0 && j<N;i--,j++)
			if(board[i][j]==1) return 0;
	for(int i=r+1,j=c-1;i<N && j>=0;i++,j--)
			if(board[i][j]==1) return 0;
	return 1;
}

/**
 * Method to place a queen at the parameter row
 */
void placeQueen(unsigned int row)
{
	if(row==N){
		//print_board();
		solutions++;
		return;
	}

	for(unsigned int col=0;col<N;col++){
		if(isSafe(row,col)){
			board[row][col] = 1;
			//state this_state = get_state();
			//states_set.insert(this_state);
			cnt++;
			placeQueen(row+1);
			board[row][col]=0; // Execution of this statement implies undo action, and continue search.
		}
	}
}

int main()
{
	//initial state: no queen on the board
	for(unsigned int i=0;i<N;i++)
		for(unsigned int j=0;j<N;j++)
			board[i][j] = 0;

	queue<position> S;
	position initial;

	initial.first=0; initial.second=0; cnt=1; //
	S.push(initial);

	while(S.size() != 0)
	{

		position last_position = S.front();
		S.pop();
		int row = last_position.first;
		int col = last_position.second;
	
		if(isSafe(row,col))
		{
			cnt++;
			board[row][col] = 1;
			
			if(row == N-1)
			{
				solutions++;
			}
		}
		
		if(row<col && col<N)
		{
			position next_pos;
			next_pos.first = row;
			next_pos.second = col+1;
			S.push(next_pos);
		}
		else if(col<row && row<N)
		{
			position next_pos;
			next_pos.first = row+1;
			next_pos.second = col;
			S.push(next_pos);
		}
		else if(col==row && row<N)
		{
			position p1, p2, p3;
			p1.first = row+1;
			p2.first = row+1;
			p3.first = row;
			p1.second = col;
			p2.second = col+1;
			p3.second = col+1;
			S.push(p1);
			S.push(p2);
			S.push(p3);
		}
		
	}
	print_board();
	std::cout << "Search complete\nTotal solutions = " << solutions << std::endl;
	std::cout << "Size of the state-space = " << cnt << std::endl;
	return 0;
}



