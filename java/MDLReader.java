/*========== MDLReader.java ==========
  MDLReader objects minimally contain an ArrayList<opCode> containing
  the opCodes generated when an mdl file is run through the java created
  lexer/parser, as well as the associated SymTab (Symbol Table).

  The provided methods are a constructor, and methods to print out the
  entries in the symbol table and command ArrayList.

  Your job is to go through each entry in opCodes and perform
  the required action from the list below:

  push: push a new origin matrix onto the origin stack
  pop: remove the top matrix on the origin stack

  move/scale/rotate: create a transformation matrix
  based on the provided values, then
  multiply the current top of the
  origins stack by it.

  box/sphere/torus: create a solid object based on the
  provided values. Store that in a
  temporary matrix, multiply it by the
  current top of the origins stack, then
  call draw_polygons.

  line: create a line based on the provided values. Store
  that in a temporary matrix, multiply it by the
  current top of the origins stack, then call draw_lines.

  save: save the current screen with the provided filename

  =========================*/

import java.util.*;
import java.io.*;
import java.awt.Color;

import parser.*;
import parseTables.*;

public class  MDLReader {

    ArrayList<opCode> opcodes;
    SymTab symbols;
    Set<String> symKeys;

    Color c;
    Screen s;
    EdgeMatrix edges;
    PolygonMatrix polys;

    GfxVector view;
    Color ambient;
    GfxVector lightPos;
    Color lightColor;

    Matrix transform;
    Stack<Matrix> csystems;

    public MDLReader(ArrayList<opCode> o, SymTab st) {

		opcodes = o;
		symbols = st;
		symKeys = st.keySet();

		c = Color.GREEN;
		s = new Screen();
		edges = new EdgeMatrix();
		polys = new PolygonMatrix();

		view = new GfxVector(0, 0, 1);
		ambient = new Color(50, 50, 50);
		lightPos = new GfxVector(0.5, 0.75, 1);
		lightColor = new Color(255, 255, 255);

		transform = new Matrix();
		transform.ident();
		csystems = new Stack<Matrix>();
		csystems.push(transform);
    }//constructor

    public void printCommands() {
		Iterator i = opcodes.iterator();
		while (i.hasNext()) {
			System.out.println(i.next());
	    }
    }//printCommands

    public void printSymbols() {
		Iterator i;

		i = symKeys.iterator();
		System.out.println("Symbol Table:");
		while (i.hasNext()) {
			String key = (String)i.next();
			Object value=symbols.get(key);
			System.out.println(""+key+"="+value);
		}
    }//printSymbols

    /*======== public void process()) ==========
      Inputs:
      Returns:

      Insert your interpreting code here

      oc.getName() will return the class name of the
      opCode object (i.e. "opBox").

      you will need to typecast in order to get the
      operation specific data values (i.e. (opBox)oc)
      ====================*/
    public void process() {

		Iterator i = opcodes.iterator();
		opCode oc;
		double step2d = 0.01;
		int step3d = 100;


		opConstants white = new opConstants("_white",
											new double[]{0.2, 0.2, 0.2},
											new double[]{0.5, 0.5, 0.5},
											new double[]{0.5, 0.5, 0.5},
											null);
		opConstants reflect = white;

		while (i.hasNext()) {

			oc = (opCode)i.next();
			String command = oc.getName();
			System.out.println(command);

			if ( command.equals("opPush") ) {
				csystems.push( csystems.peek().copy() );
			}//push

			else if ( command.equals("opPop") ) {
				csystems.pop();
			}//pop

			else if ( command.equals("opSphere") ) {
				opSphere os = (opSphere)oc;
				polys.addSphere( os.getCenter()[0],
								 os.getCenter()[1],
								 os.getCenter()[2],
								 os.getR(), step3d);
				polys.mult(csystems.peek());

				if ( os.getConstants() != null ) {
					reflect = (opConstants)(symbols.get( os.getConstants() ));
				}//retrieve constants
				polys.drawPolygons(s, view, ambient, lightPos, lightColor, reflect);
				polys.clear();
				reflect = white;
			}//sphere

			else if ( command.equals("opTorus") ) {

				opTorus ot = (opTorus)oc;
				polys.addTorus( ot.getCenter()[0],
								ot.getCenter()[1],
								ot.getCenter()[2],
								ot.getr(),
								ot.getR(), step3d);
				polys.mult(csystems.peek());
				if ( ot.getConstants() != null ) {
					reflect = (opConstants)(symbols.get( ot.getConstants() ));
				}//retrieve constants
				polys.drawPolygons(s, view, ambient, lightPos, lightColor, reflect);
				polys.clear();
				reflect = white;
			}//torus

			else if ( command.equals("opBox")) {
				opBox ob = (opBox)oc;
				polys.addBox( ob.getP1()[0],
							  ob.getP1()[1],
							  ob.getP1()[2],
							  ob.getP2()[0],
							  ob.getP2()[1],
							  ob.getP2()[2] );
				polys.mult(csystems.peek());
				if ( ob.getConstants() != null ) {
					reflect = (opConstants)(symbols.get( ob.getConstants() ));
				}//retrieve constants
				polys.drawPolygons(s, view, ambient, lightPos, lightColor, reflect);
				polys.clear();
				reflect = white;
			}//box

			else if ( command.equals("opLine") ) {

				opLine ol = (opLine)oc;
				edges.addEdge( ol.getP1()[0],
							   ol.getP1()[1],
							   ol.getP1()[2],
							   ol.getP2()[0],
							   ol.getP2()[1],
							   ol.getP2()[2] );
				edges.mult(csystems.peek());
				edges.drawEdges(s, c);
				polys.clear();
			}//line

			else if ( command.equals("opMove") ) {
        opMove om = (opMove)oc;
				Matrix tmp = new Matrix(Matrix.TRANSLATE,
										om.getValues()[0],
										om.getValues()[1],
										om.getValues()[2] );
				tmp.mult(csystems.pop());
				csystems.push(tmp);
			}//move

			else if ( command.equals("opScale") ) {
        opScale osc = (opScale)oc;
				Matrix tmp = new Matrix(Matrix.SCALE,
										osc.getValues()[0],
										osc.getValues()[1],
										osc.getValues()[2] );
				tmp.mult(csystems.pop());
				csystems.push(tmp);
			}//scale

			else if ( command.equals("opRotate") ) {
        opRotate or = (opRotate)oc;
				double angle = or.getDeg() * (Math.PI / 180);
				char axis = or.getAxis();
				Matrix tmp = new Matrix(Matrix.ROTATE, angle, axis);
				tmp.mult(csystems.pop());
				csystems.push(tmp);
			}//rotate

			else if ( command.equals("opSave") ) {
				s.saveExtension( ((opSave)oc).getName() );
			}//save
			else if ( command.equals("opDisplay") ) {
				s.display();
			}//save
		}//end loop
	}//process

}//MDLReader
