##the parameters given på the task
g = 2
p = 107

Key_1 = 15
Key_2 = 20

##creating the public keys
pub1 = g**Key_1 % p
pub2 = g**Key_2 % p

##creating the private keys
priv1 = pub2**Key_1 % p
priv2 = pub1**Key_2 % p

print(
    f"""
public key: {pub1,pub2}
shared key: {priv1,priv2} 

thbar6999
      """
)
