#include <iostream>
#include <stack>
#include <utility>
#define N 4
unsigned int board[N][N];
unsigned int solutions=0,cnt=1;
typedef std::pair<int, int> position;
using namespace std;
/* Method that checks if the current action keeps the board safe, that is non-attacking. */
unsigned int isSafe(unsigned int r, unsigned int c)
{
	//vertical safe check
	for(unsigned int j=0;j<N;j++)
		if(j!=c && board[r][j]==1) return 0;
	// horizontal safety check
	for(unsigned int i=0;i<N;i++)
		if(i!=r && board[i][c]==1) return 0;
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
/* Method to place a queen at the parameter row */
void placeQueen(unsigned int row)
{
	if(row==N)
	{
		solutions++;
		return;
	}
	for(unsigned int col=0;col<N;col++){
		if(isSafe(row,col)){
			board[row][col] = 1;
			cnt++;
			placeQueen(row+1);
			board[row][col]=0; // Execution of this statement implies undo action, and continue search.
		}
	}

int main()
{
	//initial state: no queen on the board
	for(unsigned int i=0;i<N;i++)
		for(unsigned int j=0;j<N;j++)
			board[i][j] = 0;
	stack<position> S;
	position initial;
	initial.first=0; initial.second=-1; cnt=1;
	S.push(initial);
	while(!S.empty())
	{
		position last_position = S.top();
		S.pop();
		int row = last_position.first;
		int col = last_position.second;
		if(col>=0)
			board[row][col] = 0; // take-off the queen from this position
		for(int j=col+1;j<N;j++)
		{
			if(isSafe(row,j))
			{
				cnt++;
				board[row][j] = 1; // new position to insert queen in this row
				position new_pos;
				new_pos.first = row;
				new_pos.second = j;
				S.push(new_pos);
				if(row==N-1)
					solutions++;
				else
				{
					// add next row to search for a safe position in the board
					position next_pos;
					next_pos.first = row+1;
					next_pos.second = -1;
					S.push(next_pos);
				}
				break; // move to next level of the search tree
			}
		}
	}
	std::cout << "Search complete\nTotal solutions = " << solutions << std::endl;
	std::cout << "Size of the state-space = " << cnt << std::endl;
	return 0;
}
