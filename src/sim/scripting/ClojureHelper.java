package sim.scripting;

public class ClojureHelper {

	/*
		cell (fn [^sim.field.grid.IntGrid2D agrid x y dx dy] 
				 (aget (.field agrid) 
				       (.stx agrid (+ x dx)) 
				       (.sty agrid (+ y dy))))
	*/
	
	public static int cell(sim.field.grid.IntGrid2D agrid, int x, int y, int dx, int dy) {
		return agrid.field[agrid.stx(x + dx)][agrid.sty(y + dy)];
	}
	
	public static int lcount(sim.field.grid.IntGrid2D agrid, int x, int y) {
		return cell(agrid, x, y, -1, -1) + cell(agrid, x, y,  0, -1) + cell(agrid, x, y,  1, -1) + 
			   cell(agrid, x, y, -1,  0) + cell(agrid, x, y,  0,  0) + cell(agrid, x, y,  1,  0) + 
			   cell(agrid, x, y, -1,  1) + cell(agrid, x, y,  0,  1) + cell(agrid, x, y,  1,  1);
			  
	}
	
	public static void set(sim.field.grid.IntGrid2D agrid, int x, int y, int value) {
		agrid.field[x][y] = value;
	}
}
