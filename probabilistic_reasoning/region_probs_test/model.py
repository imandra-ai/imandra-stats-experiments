class State:
    def __init__(self):
        self.x : int = 1
        
    def receive_Update(self, newx : int , errint : int ):
        if newx > self.x:
            self.x : int  = newx
        else:
            self.x : int  = errint