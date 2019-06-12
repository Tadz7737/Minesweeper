import Array._
import scala.collection.mutable.ListBuffer
import scala.util.Random
import java.awt.BorderLayout
import java.awt.Dimension
import javax.swing._
import javax.swing.border.LineBorder;
import java.awt.Color;

object Field{
    val matrixDim = 30;
    val minesCount = 100;
    val fieldSize = 25;
    var board: Array[Array[Field]] = Array.ofDim[Field](matrixDim,matrixDim)
    private var pressedCount = matrixDim * matrixDim - minesCount;
    private def pressedCountDec = {pressedCount -= 1}
    private var flagsCount = minesCount;
    private def flagsCountInc = {flagsCount += 1}
    private def flagsCountDec = {flagsCount -= 1}
    private def checkNeighbors(xC: Int, yC: Int){
        board(xC)(yC).isNeighborChecked = true;
        if(xC-1 >= 0 && !board(xC-1)(yC).isPressed && !board(xC-1)(yC).isMine && !board(xC-1)(yC).isNeighborChecked){
                    setNeighbor(xC-1, yC)
                    if(board(xC-1)(yC).minesNear == 0)
                        checkNeighbors(xC-1, yC)
                    else
                        board(xC-1)(yC).isNeighborChecked = true
        }
        if(yC-1 >= 0 && !board(xC)(yC-1).isPressed && !board(xC)(yC-1).isMine && !board(xC)(yC-1).isNeighborChecked){
                    setNeighbor(xC, yC-1)
                    if(board(xC)(yC-1).minesNear == 0)
                        checkNeighbors(xC, yC-1)
                    else
                        board(xC)(yC-1).isNeighborChecked = true
        }
        if(xC+1 < matrixDim && !board(xC+1)(yC).isPressed && !board(xC+1)(yC).isMine && !board(xC+1)(yC).isNeighborChecked){
                    setNeighbor(xC+1, yC)
                    if(board(xC+1)(yC).minesNear == 0)
                        checkNeighbors(xC+1, yC)
                    else
                        board(xC+1)(yC).isNeighborChecked = true
        }
        if(yC+1 < matrixDim && !board(xC)(yC+1).isPressed && !board(xC)(yC+1).isMine && !board(xC)(yC+1).isNeighborChecked){
                    setNeighbor(xC, yC+1)
                    if(board(xC)(yC+1).minesNear == 0)
                        checkNeighbors(xC, yC+1)
                    else
                        board(xC)(yC+1).isNeighborChecked = true
        }

    }
    private def setNeighbor(xC: Int, yC: Int){
            board(xC)(yC).isPressed = true
            board(xC)(yC).button.setBackground(Color.GREEN);
            if(board(xC)(yC).minesNear==0)
                board(xC)(yC).button.setText("");
            else
                board(xC)(yC).button.setText(""+board(xC)(yC).minesNear);    

            Field.pressedCountDec
            if(Field.pressedCount == 0){
                JOptionPane.showMessageDialog(null, "Congratulations! You won.");            
                System.exit(0);
            }
    }
}

class Field(imXC: Int, imYC: Int){
    var xC: Int = imXC;
    var yC: Int = imYC;
    val fieldSize = Field.fieldSize
    var corX: Int = imXC * fieldSize;
    var corY: Int = imYC * fieldSize;
    var height: Int = fieldSize;
    var widht: Int = fieldSize;
    var isMine: Boolean = false;
    var isFlagged: Boolean = false;
    var isPressed: Boolean = false;
    var isNeighborChecked: Boolean = false;
    var minesNear: Int = 0;
    var button: JButton = new JButton();
    button.setBounds(corX, corY, widht, height);
    button.setBorder(BorderFactory.createLineBorder(Color.black, 2))
    button.addActionListener(new java.awt.event.ActionListener() { 
        def actionPerformed(e: java.awt.event.ActionEvent) { 
            buttonPressed();
        } 
    } );
    button.addMouseListener(new java.awt.event.MouseAdapter(){
        override def mouseClicked(e: java.awt.event.MouseEvent){
            if (e.getButton() == java.awt.event.MouseEvent.BUTTON3)
                if(isFlagged)
                    removeFlag()
                else if(!isPressed){
                        setFlag()
                        isFlagged = true
                }
            //delete actionlistener and add code for mouse event button 1
        }
    } );
    def buttonPressed(){
        this.isPressed = true;
        if(this.isMine){
            button.setBackground(Color.RED);
            button.setText("x");
            JOptionPane.showMessageDialog(null, "Boom! You lost.");            
            System.exit(0);
        }
        else{
            button.setBackground(Color.GREEN);
            if(this.minesNear==0)
                button.setText("");
            else
                button.setText(""+this.minesNear);

            Field.pressedCountDec
            if(Field.pressedCount == 0){
                JOptionPane.showMessageDialog(null, "Congratulations! You won.");            
                System.exit(0);
            }
            if(!isNeighborChecked)
                Field.checkNeighbors(xC,yC);
        }
    }
    def setFlag(){
          if(Field.flagsCount>0){
            button.setText("F");
            button.setBackground(Color.YELLOW);
            isFlagged = true;
            Field.flagsCountDec
          }
    }
    def removeFlag(){
        this.isFlagged = false;
        Field.flagsCountInc
        if(this.isPressed){
            button.setBackground(Color.GREEN);
            if(this.minesNear==0)
                    button.setText("");
                else
                    button.setText(""+this.minesNear);        
        }else{
            button.setBackground(null);
            button.setText("");
        }
        
    }
    def checkNeighbors(){
        
    }
}

object Board extends App{
  
    override def main(args: Array[String]){
         val matrixDim = Field.matrixDim
         val minesCount = Field.minesCount
         val board = Field.board
         val fieldSize = Field.fieldSize
    
         val frame = new JFrame("Minesweeper")

         frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
         frame.setLocationRelativeTo(null)
         frame.setVisible(true)
         frame.setResizable(false);
         frame.setSize(new Dimension(matrixDim*fieldSize, matrixDim*fieldSize))
         fill(board)
         fillWithMines(board,minesCount)
         fillWithNumbers(board)
         printBoard(board)
         addButtons(board, frame)
         frame.repaint()
         frame.addWindowListener(
            new java.awt.event.WindowAdapter() { 
                override def windowClosing(
                windowEvent : java.awt.event.WindowEvent ) {
                if (JOptionPane.showConfirmDialog(frame, 
                    "Are you sure you want to end the game?", "End game?", 
                    JOptionPane.YES_NO_OPTION,
                    JOptionPane.QUESTION_MESSAGE) == JOptionPane.YES_OPTION){
                    System.exit(0);
                }
            }
        });
    }

    def printBoard(imBoard: Array[Array[Field]]){
        for(i <- 0 until imBoard.length){
             for(j <- 0 until imBoard.length){
                if(imBoard(j)(i).isMine)
                    print("X ")
                else
                    print(imBoard(j)(i).minesNear+" ")
             }
             println
         }
    }
    def fill(imBoard: Array[Array[Field]]){
        for(i <- 0 until imBoard.length; j <- 0 until imBoard.length){
             imBoard(i)(j) = new Field(i,j);
         }
    }

    def fillWithMines(imBoard: Array[Array[Field]], imMinesCount: Int){
        var boardList = new ListBuffer[Field]()
        for(i <- 0 until imBoard.length; j <- 0 until imBoard.length){
            boardList += imBoard(i)(j)
        }
        boardList = Random.shuffle(boardList)
        for(i <- 0 until imMinesCount){
            boardList(i).isMine = true
        }

    }
  
    def fillWithNumbers(imBoard: Array[Array[Field]]){
        var tmpBoard: Array[Array[Field]] = Array.ofDim[Field](imBoard.length+2,imBoard.length+2)
        fill(tmpBoard)
        for(i <-1 until tmpBoard.length-1){
            for(j <-1 until tmpBoard.length-1){
                tmpBoard(i)(j) = imBoard(i-1)(j-1)
            }
        }

        for(i <- 1 until tmpBoard.length-1){
             for(j <- 1 until tmpBoard.length-1){
                 //i - col j - row
                if(tmpBoard(i-1)(j-1).isMine)
                    tmpBoard(i)(j).minesNear += 1;
                if(tmpBoard(i)(j-1).isMine)
                    tmpBoard(i)(j).minesNear += 1;
                if(tmpBoard(i+1)(j-1).isMine)
                    tmpBoard(i)(j).minesNear += 1;
                if(tmpBoard(i-1)(j).isMine)
                    tmpBoard(i)(j).minesNear += 1;
                if(tmpBoard(i+1)(j).isMine)
                    tmpBoard(i)(j).minesNear += 1;
                if(tmpBoard(i-1)(j+1).isMine)
                    tmpBoard(i)(j).minesNear += 1;
                if(tmpBoard(i)(j+1).isMine)
                    tmpBoard(i)(j).minesNear += 1;
                if(tmpBoard(i+1)(j+1).isMine)
                    tmpBoard(i)(j).minesNear += 1;
             }
         }
     }

    def addButtons(imBoard: Array[Array[Field]],frame: JFrame){
        for(i <- 0 until imBoard.length; j <- 0 until imBoard.length)
            frame.add(imBoard(i)(j).button);
    }
}