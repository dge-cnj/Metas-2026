# 📊 Metas Nacionais do Poder Judiciário – 2026

## Apresentação

As Metas Nacionais do Poder Judiciário para o ano de 2026 foram aprovadas pelos presidentes e representantes dos tribunais brasileiros durante o **19º Encontro Nacional do Poder Judiciário**, realizado nos dias **1º e 2 de dezembro de 2025**, em Florianópolis/SC. Conforme previsto no artigo 13 da Resolução CNJ nº 325/2020, a **Meta Nacional 1** integra obrigatoriamente o monitoramento da Estratégia Nacional do Poder Judiciário 2021–2026, não sendo submetida à votação pelos tribunais. As Metas Nacionais representam o compromisso institucional da Justiça brasileira com o aprimoramento contínuo da prestação jurisdicional, promovendo maior eficiência, celeridade, transparência e qualidade dos serviços oferecidos à sociedade.

## 🎯 Metas Nacionais do Poder Judiciário

Definidas anualmente pelo Conselho Nacional de Justiça (CNJ) e aprovadas durante o Encontro Nacional do Poder Judiciário, as Metas Nacionais orientam a atuação estratégica dos tribunais brasileiros.

Entre os principais objetivos para 2026 destacam-se:

- Julgar mais processos do que os distribuídos;
- Reduzir a taxa de congestionamento processual;
- Priorizar o julgamento de processos mais antigos;
- Estimular a solução consensual de conflitos;
- Promover a inovação e a transformação digital;
- Priorizar ações relacionadas à sustentabilidade e ao meio ambiente;
- Dar tratamento prioritário a processos envolvendo povos indígenas e comunidades quilombolas;
- Fortalecer o enfrentamento à violência doméstica e ao feminicídio.

## ✨ Sobre o Projeto

Este repositório reúne scripts e recursos desenvolvidos para apoiar o processamento, consolidação e análise dos dados utilizados no acompanhamento das Metas Nacionais do Poder Judiciário.

As principais funcionalidades contempladas são:

- Extração e tratamento de dados;
- Consolidação das bases do DataJud;
- Cálculo dos indicadores das Metas Nacionais;
- Geração de relatórios e arquivos de apoio;
- Automatização de rotinas estatísticas;
- Suporte ao monitoramento estratégico dos tribunais.

## 📁 Estrutura do Repositório

| Pasta | Descrição |
|--------|------------|
| `Glossarios/` | Glossários oficiais utilizados no ciclo das Metas Nacionais 2026. |
| `ScriptCompleto/` | Scripts destinados ao processamento consolidado de todos os tribunais. |
| `ScriptTribunal/` | Scripts destinados ao processamento individual de um tribunal específico. |

## ⚠️ Observação Importante

Para a execução dos scripts localizados na pasta `ScriptTribunal/`, os arquivos gerados pelo processo **Extrai Datamart** devem estar armazenados no mesmo diretório dos códigos-fonte em R.

## 📌 Tecnologias Utilizadas

| Pacote | Finalidade |
|---------|------------|
| `data.table` | Manipulação de grandes volumes de dados com alto desempenho. |
| `dtplyr` | Integração entre as sintaxes do dplyr e data.table. |
| `lubridate` | Manipulação simplificada de datas e horários. |
| `tidyverse` | Conjunto de pacotes para análise e transformação de dados. |
| `winch` | Suporte à depuração e análise de pilhas de erro. |
| `doParallel` | Execução paralela utilizando clusters. |
| `foreach` | Estruturas iterativas e paralelas. |
| `processx` | Gerenciamento de processos externos. |
| `writexl` | Exportação simplificada de arquivos Excel. |
| `future` | Infraestrutura para processamento assíncrono e paralelo. |
| `Rcpp` | Integração entre R e C++ para ganho de desempenho. |
| `openxlsx` | Leitura e escrita avançada de arquivos Excel. |

## 🚀 Fluxo Geral de Processamento

| Etapa | Descrição |
|--------|------------|
| 📥 1 | Extração dos dados do DataJud |
| 🔄 2 | Tratamento, limpeza e padronização |
| ⚙️ 3 | Aplicação das regras das Metas Nacionais |
| 📊 4 | Cálculo dos indicadores |
| 📑 5 | Geração de relatórios e arquivos de saída |
| 📈 6 | Construção de painéis e acompanhamento dos resultados |

## 🔗 Fontes Oficiais

As informações oficiais sobre as Metas Nacionais podem ser consultadas nos seguintes endereços:

- Portal das Metas Nacionais do CNJ
- Painel das Metas Nacionais 2026
- Glossário das Metas Nacionais 2026

---

## 📖 Referências

- [Conselho Nacional de Justiça (CNJ)](https://www.cnj.jus.br/)
- [Resolução CNJ nº 325/2020](https://atos.cnj.jus.br/atos/detalhar/3365)
- [Estratégia Nacional do Poder Judiciário 2021–2026](https://www.cnj.jus.br/wp-content/uploads/2020/09/estrategia-nacional-poder-judiciario-2021-2026.pdf)
- [Portal das Metas Nacionais](https://www.cnj.jus.br/gestao-estrategica-e-planejamento/metas/)
- [Metas Nacionais 2026](https://www.cnj.jus.br/gestao-estrategica-e-planejamento/metas/metas-2026/)
- [Painel das Metas Nacionais](https://justica-em-numeros.cnj.jus.br/painel-metas/)

## ℹ️ Aviso

Os scripts disponibilizados neste repositório possuem finalidade técnica e institucional, destinando-se ao apoio das atividades de processamento, cálculo e acompanhamento das Metas Nacionais do Poder Judiciário. Os resultados produzidos dependem da qualidade e integridade dos dados fornecidos pelos tribunais ao DataJud e devem ser interpretados em conjunto com os normativos, glossários e orientações oficiais publicados pelo Conselho Nacional de Justiça.

## 📄 Licença

Este repositório destina-se exclusivamente a fins institucionais, acadêmicos e de apoio às atividades de monitoramento das Metas Nacionais do Poder Judiciário.

**Última atualização:** Maio/2026
