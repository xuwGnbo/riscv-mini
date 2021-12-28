int cntSum(int num);
void exit();

void main(){
	cntSum(5);
	exit();
}

int cntSum(int num){
	if(num == 1) return num;
	else return num + cntSum(num - 1);
}

void exit(){
	while(1){
		asm("csrw mtohost, 1");
	}
}
