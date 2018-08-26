#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>

/*
fork() サーバプロセスを fork して並行サーバ化
select() 一つのサーバプロセス内でファイルディスクリプタを一つずつ見ていく
thread() 一つのサーバプロセス内でスレッドを複数たてて並行サーバ化
*/

int server_socket; // サーバプロセスのソケット
int client_socket;
int client_len;
char* client_addr_str;
struct sockaddr_in server_addr; // このサーバの情報
struct sockaddr_in client_addr; // クライアントの情報

int main(int argc, char *argv[])
{
  server_addr.sin_family = AF_INET; // IPv4 を使う
  server_addr.sin_port = htons(12345); // 12345 ポートで待つ
  server_addr.sin_addr.s_addr = INADDR_ANY; // 通信相手は誰でもよい

  // ソケットを作る
  server_socket = socket(AF_INET, SOCK_STREAM, 0);

  // サーバプロセスをポートに結びつける
  bind(server_socket, (struct sockaddr *)&server_addr, sizeof(server_addr));

  // ソケットをlisten状態にする
  listen(server_socket, 5);

  while(1) {
    client_len = sizeof(client_addr);
    // クライアントの connect() がきたらそれ用のソケットを作って返す
    client_socket = accept(server_socket, (struct sockaddr *)&client_addr, &client_len);
    printf("accepted connection from %s, port=%d\n",
           (char *)inet_ntop(AF_INET,
                             &client_addr.sin_addr,
                             client_addr_str,
                             sizeof(client_addr_str)),
           ntohs(client_addr.sin_port));
    write(client_socket, "Hello\n", 7);
    close(client_socket);
  }
}
