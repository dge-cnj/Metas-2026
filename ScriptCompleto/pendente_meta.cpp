#include <Rcpp.h>
using namespace Rcpp;


//Função pendente_meta: verifica, com base nos valores de array_pendente, se o processo tramitou na data_limite
//#Parâmetros: array_pendente, que é uma coluna do datamart que mostra o tempo que o processo tramitou. No caso das metas será bastante utilizada a coluna dt_pendente_liquido.
//#            data_limite, no padrão yyyymmdd, que é a data que deseja verificar se o processo estava tramitando. No caso das metas será utilizada a data indicada em ref.
//#Valor de retorno: uma string no formato yyyymmdd:yyyymmdd que indica o período que o processo tramitou na data_limite
//#Versão: 29/04/2025


// [[Rcpp::export]]
std::string pendente_meta_cpp(std::vector<std::string> intervalos, int data_limite) {
  std::string ultimo_valor = "NA";
  for (int i = intervalos.size() - 1; i >= 0; --i) {
    std::string intv = intervalos[i];
    size_t pos = intv.find(":");
    if (pos == std::string::npos) continue;

    int inicio = std::stoi(intv.substr(0, pos));
    int fim = std::stoi(intv.substr(pos + 1));

    if (inicio < data_limite && fim >= data_limite) {
      return intv;
    } else if (inicio < data_limite && fim < data_limite) {
      return intv;
    }

    ultimo_valor = intv;
  }

  return ultimo_valor;
}
