def leertxt():
  archiO=open('datosIn.txt','r')
  archi=open('datosOut.txt','a')

  linea=archiO.readline()
  while linea!="":
    print linea
    archi.write(linea[3:])
    linea=archiO.readline()
  archiO.close()
  archi.close()

leertxt()
