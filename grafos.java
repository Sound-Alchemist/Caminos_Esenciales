// BUAP
// FCC - M.C. Pedro Bello Lopez
import java.io.*;
import java.util.StringTokenizer;
class grafos
{
 int NN;
 
 int M[][] = new int[20][20];
 
 
 public void escribematriz()
 {
 	int i,j;
 	System.out.println(" NODOS = "+NN);
 	System.out.println(" Matriz de Adyacencias");
 	System.out.print(" ");
 	for(i=1;i<=NN;i++)System.out.print("  "+i);	
 	System.out.println();
 	
 	for(i=1;i<=NN; i++)
 	{
 		System.out.print(i+"  ");
 		for (j=1;j<=NN;j++)
 		System.out.print(M[i][j]+"  ");	
 		System.out.println();
 	}
 }
 
 public void LeeGrafo(String arch) //Lee archivo con los datos del grafo
  {
  	
  		FileInputStream fp;
		DataInputStream f;
		String linea = null;
		int token1,token2,i,j;
 try
     {
		fp = new FileInputStream(arch);
		f = new DataInputStream(fp);
		linea=f.readLine();
		
		NN=Integer.parseInt(linea); 
		System.out.println(" Numero de Nodos: "+NN);
		// Inicializamos la matriz con ceros
  		for (i=1;i<=NN;i++)
  		     for(j=1;j<=NN; j++)
  		        M[i][j]=0;
  		        
  		// Leemos el archivo linea por linea
		do {
			linea = f.readLine();
			if (linea!=null){
				              StringTokenizer tokens=new StringTokenizer(linea); 
				              token1 = Integer.parseInt(tokens.nextToken());
				              token2 = Integer.parseInt(tokens.nextToken());
				              // escribimos en pantalla los datos leidos transformados en numeros
				              System.out.println(token1+" "+token2);
				              // almacenamos en la matriz
				              M[token1][token2]=1;
				              M[token2][token1]=1;
				            }  
			}while(linea != null);
		fp.close();	
	}
	
	
         catch (FileNotFoundException exc)
         {
             System.out.println ("El archivo " + arch + " no fue encontrado ");
         }
         
         catch (IOException exc)
         {
             System.out.println (exc);
         }
  		
  }
  
  public static void main(String[] ar)
  {
    grafos G=new grafos();
    G.LeeGrafo("entrada.dat");
    G.escribematriz();
  }
  }