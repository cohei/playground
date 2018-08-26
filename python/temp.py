# -*- coding: utf-8 -*-
counts = [0]
r = range(0,10)
for i in raw_input().strip():
    counts += [0]
    counts[int(i)] += 1 # 現れた牌を数字ごとにカウントする 降順だとうまくいかない

def show(mentsu, machi):
    print "(" + ")(".join(mentsu) + ")" + machi # 表示関数

def f(output, cs, done, bound): # u: 出力用スタック cs: カウントのリスト p: できた面子の数
    # b: たぶんこの数牌まで処理したという意味(昇順でないと意味ない)
 for i in range(bound,10):
     if cs[i] > 2: # 同じ牌が3枚か4枚あったら
         output += [str(i)*3] # outputに3枚つめて
         cs[i] -= 3 # カウントから3枚へらす
         f(output, cs, done+1, i+1) # 残りで再帰
         cs[i] += 3
         output.pop()
     if i < 8 and cs[i] * cs[i+1] * cs[i+2] > 0: # 順子ができるなら
         output+=["".join(map(str,(i,i+1,i+2)))] # outputにつめて
         f(output, cs[:i] + [cs[i]-1, cs[i+1]-1, cs[i+2]-1] + cs[i+3:], done+1, i)
           # 残りで再帰
         output.pop()
 if done == 3: # 3つ面子ができて (あと4枚)
     h = [ x for x in r if cs[x]>1 ] # 1から9でまだ2枚以上牌があるのは
     if len(h) == 2: # 2種類*2枚だけ牌が残ってたらシャボ待ち
         q = str(h[0]) * 2
         t = str(h[1]) * 2
         show(output+[q], "["+t+"]")
         show(output+[t], "["+q+"]")
     if len(h) == 1: # 3枚+1枚？
         a = [ a for a in r if cs[a] in [1,3] ]
         if a[1] - a[0] in [1,2]: # カンチャンかリャンメン
             t = "[" + str(a[0]) + str(a[1]) + "]"
             show(output+[str(h[0])*2], t)
 if done > 3:
     show(output, str([ x for x in r if cs[x] > 0 ]))

f([],counts,0,1)
