# Metodologia

Para calcular as taxas de natalidade, fecundidade, mortalidade e
crescimento populacional foi necessário inicialmente obter a população
geral e do sexo feminino com idade entre 15 a 49 anos por bairro. A
metodologia para o cálculo da população considera a evolução do número
de habitantes por meio do balanço de registros de nascimentos nos
bairros de residências das mães e óbitos nos bairros de residências do
falecido, por meio, respectivamente, do [Sistema de Informações sobre
Nascidos Vivos](http://sinasc.saude.gov.br/default.asp) (SINASC) e do [Sistema de Informação Sobre Mortalidade](https://opendatasus.saude.gov.br/dataset/sim-2020-2021)
(SIM). Para este cálculo, desconsidera-se nesse o fluxo migratório de habitantes,
devido a dificuldade dessas informações. Pressupõe-se que os registros
populacionais tem data de referência em 1º de julho de cada ano civil.

Uma tabela `DEPARA` foi criada para relacionar a nomenclatura dos
bairros entre as fontes do censo e dos sistemas SIM e SINASC. Precisou-se
fazer duas modificações na base dos bairros do censo para seguir com o
alinhamento, contou-se com apoio da Diretoria de Planejamento (DIPLA) do IPLANFOR:

1.  Unificou-se a população dos bairros `Gentilandia` e `Benfica`, por se entender
    que se trata de um subconjunto e, também, por não constar na base da
    Secretaria Municipal de Saúde (SMS).
2.  Excluiu-se `São Bento` por não constrar na base da SMS.

## Cálculo Populacional

Seja a notação $P_{x,i}$ representando a população no bairro $x$ e no
ano $i$, tem-se que $P_{x,i+1}$ é dado por:

$$P_{x,i+1} = P_{x,i} + \frac{B_{x,i+1}+B_{x,i}}{2}$$

Em que $B_{x,i}$ repesenta o balanço entre os registros de nascimentos e
óbitos no bairro $x$ e ano $i$, dado por:

$$B_{x,i} = N_{x,i} - O_{x,i},$$ 

sendo $N_{x,i}$ e $O_{x,i}$ o número de nascimentos e o número de óbitos no bairro $x$ e ano $i$ respectivamente.

Dessa forma, a população no bairro em um determinado ano é dada pela
população do ano anterior mais o incremento da média da diferença entre
os nascimentos e óbitos (balanço) do ano atual e anterior. Esse
incremento adicionado advém do fato de se obter uma aproximação do
balanço populacional, através de nascimentos e óbitos, ocorridos entre o
meio e o fim do ano anterior e os ocorridos entre o início e o meio do
determinado ano. A população por bairro obtida no censo demográfico de
2010 foi usada para estimar $P_{x, 2010}$. Para os demais anos,
utilizou-se a fórmula.

## Cálculo da população feminina de 15 a 49 anos de idade

Seja a notação $P_{x,i}^{F15,49}$ representando a população feminina
entre 15 e 49 anos de idade no bairro $x$ e no ano $i$, e
desconsiderando o fluxo migratório de habitantes, tem-se que:

$$P_{x,i}^{F15,49} = P_{x,i-1}^{F14,48}-{O_{i-1}^{14,48}+O_{i}^{15,49}\over 2};$$

Isto é, a população feminina entre 15 e 49 anos de idade no ano $i$
será a população de 14 a 48 anos de idade do ano anterior $i-1$ menos
as perdas populacionais, registradas em óbitos, que ocorrem em dois
períodos: do meio até o final do ano $i-1$ de quem tem entre 14 e 48
anos e do início até o meio do ano $i$ de que tem entre 15 a 49 anos. A
divisão por dois em cada termo representando a quantidade de óbitos é
devido ao fato de se pressupor que essa quantidade se distribui de
maneira uniforme entre os dois semestre do ano. Como os dados das
quantidades de óbitos são anuais, e sendo o interesse em apenas um
semestre, faz-se necessário a divisão por dois.

## Cálculo da Taxa de Fecundidade Geral

Representa uma estimativa do número médio de filhos nascidos vivos para
cada mil mulheres dentro do período reprodutivo ou em idade fértil (15 a
49 anos), residente em determinado espaço geográfico e ano considerado. Representa a condição reprodutiva média de mulheres.
Seja a notação $TFG_{x,i}$ representando a Taxa de Fecundidade Geral no
bairro $x$ e ano $i$, tem-se que<a id="fnr.1" href="#fn.1"><sup>1</sup></a>:

$$TFG_{x,i} = \frac{N_{x,i}}{P_{x,i}^{F15,49}}\times 1000$$


## Cálculo da Taxa Bruta de Natalidade

Representa o número de nascidos vivos, por mil habitantes, na população residente em determinado espaço geográfico, no ano considerado. Expressa a intensidade com a qual a natalidade atua sobre uma determinada população.
Seja a notação $TBN_{x,i}$ representando a Taxa Bruta de Natalidade no
bairro $x$ e ano $i$, tem-se que $^1$:

$$TBN_{x,i} = \frac{N_{x,i}}{P_{x,i}} \times 1000$$

## Cálculo da Taxa Bruta de Mortalidade

Representa o número total de óbitos, por mil habitantes, na população residente em determinado espaço geográfico, no
ano considerado. Seja a notação $TBM_{x,i}$ representando a Taxa Bruta de Mortalidade no bairro $x$ e ano $i$, tem-se que<a id="fnr.1" href="#fn.1"><sup>1</sup></a>:

$$TBM_{x,i} = \frac{O_{x,i}}{P_{x,i}} \times 1000$$

## Cálculo da Taxa de Crescimento Populacional Relativo

Representa a variação percentual da população residente em determinado espaço geográfico, no período considerado, em relação ao ano anterior.  Seja a notação $TCPR_{x,i}$ representando a Taxa de Crescimento Populacional Relativo no bairro $x$ e ano $i$. A fórmula de $TCPR_{x,i}$ é dada por:

$$TCPR_{x,i} = \frac{P_{x,i}}{P_{x,i-1}} - 1$$



## Base de dados

Utilizou-se neste estudo as bases de dados do Censo 2010 (IBGE), do
Sistema de Informações sobre Nascidos Vivos (SINASC) e do Sistema de
Informação Sobre Mortalidade (SIM).

### 1. CENSO Demográfico 2010 

*Para população nos bairros de Fortaleza em 2010.*

Fonte: IBGE - Censo Demográfico - Dados do Universo.

-   <https://sidra.ibge.gov.br/tabela/1378>

Parâmentros de consulta:

Variável: População residente; Situação do domicílio: Total; Sexo:
Total; Idade: Total; Condição no domicílio e o compartilhamento da
responsabilidade pelo domicílio: Total; Ano:2010; Unidade Territorial:
Fortaleza/Bairros.


### 2. SINASC - Sistema de Informação sobre Nascidos Vivos 

*Para registro de nascimento por bairro de residencia da mãe.*

Fonte: CÉLULA DE SISTEMAS DE INFORMAÇÃO E ANÁLISE EM SAÚDE - CEINFA

-   <http://tabnet.sms.fortaleza.ce.gov.br/scripts/deftohtm.exe?nascido.def>

NASCIDOS VIVOS - SINASC-Fortaleza-CE Cobertura: Total de Declarações de
Nascidos Vivos emitidas em Fortaleza digitadas até às 20h do último dia
útil. Incluem nascimentos ocorridos em Fortaleza de residentes locais e
do interior. (Dados sujeitos a revisão)

Parâmentros de consulta:

Linha: Bair.Res.Mãe; Coluna: Ano do Nascimento; Conteúdo: Nascimentos;
Períodos disponíveis: 2010 a 2020.

#### Como são coletados os dados que alimentam o SINASC

O documento padrão de uso obrigatório em todo o território nacional e
essencial à coleta de dados de nascidos vivos no Brasil é a DECLARAÇÃO
DE NASCIDOS VIVOS (DN), considerado como documento hábil para os fins do
Art. 51 da Lei nº 6.015/1973, para a lavratura da Certidão de Nascimento
pelo Cartório de Registro Civil (Art. 11 da Portaria nº 116 MS/SVS/2009)
e do inciso IV do Art. 10 da Lei nº 8.069/1990.

A Declaração de Nascidos Vivos é impressa e preenchida em três vias
pré-numeradas sequencialmente. Sua emissão e distribuição para os
estados são de competência exclusiva do Ministério da Saúde. A
distribuição para os municípios fica a cargo das Secretarias Estaduais
de Saúde. Às Secretarias Municipais de Saúde cabe o controle na
distribuição das DN entre os estabelecimentos de saúde, Cartórios do
Registro Civil, a Portaria nº 116 MS/SVS/2009 prevê também a
distribuição de formulários para profissionais de saúde e parteiras
tradicionais (estas apenas quando reconhecidas e vinculadas a unidade de
saúde), que realizem atendimento a parto domiciliar, mediante
cadastramento e controle da Secretaria Municipal de Saúde. 


#### Como são obtidos e processados os dados do SINASC

A mais recente publicação sobre coleta de dados, fluxo e periodicidade de envio das informações acerca dos nascidos vivos, e também dos óbitos, para os Sistemas de Informações geridos pela Secretaria de Vigilância à Saúde é a Portaria SVS nº 116/2009, de 11/02/2009. Esta Portaria revoga a Portaria SVS nº 20/2003.

As Declarações de Nascidos Vivos (DN) são preenchidas pelos dos profissionais de saúde, ou parteiras tradicionais responsáveis pela assistência ao parto ou ao recém-nascido (reconhecidas e vinculadas a unidades de Saúde), no caso dos partos hospitalares ou domiciliares com assistência e recolhidas, regularmente, pelas Secretarias Municipais de Saúde.

Nas Secretarias Municipais de Saúde (SMS), as Declarações de Nascidos Vivos são digitadas, processadas, criticadas e consolidadas no SINASC local. Em seguida, os dados informados pelos municípios sobre os nascimentos no nível local são transferidos à base de dados do nível estadual que os agrega e envia-os ao nível federal. Tais transferências são realizadas via WEB (internet) e ocorrem, simultaneamente, nos três níveis de gestão. No nível federal, a SVS - gestora do SINASC - conta, na sua estrutura funcional, com a Coordenação-Geral de Informações e Análises Epidemiológicas (CGIAE). Subordinada ao Departamento de Análise de Saúde e Vigilância de Doenças Não Transmissíveis (DASNT), a CGIAE trata da análise, avaliação e distribuição das informações sobre o SINASC, agregando-as por Estado, e elaborando relatórios analíticos, painéis de indicadores e outros instrumentos estatísticos de informações sobre natalidade que são disseminados para todo o país.

### 3. SIM - Sistema de Informação Sobre Mortalidade 

*Para registro de óbitos por bairro de residência do falecido*

Fonte:COORDENAÇÃO DE VIGILÂNCIA À SAÚDE - COVIS

-   <http://tabnet.sms.fortaleza.ce.gov.br/scripts/deftohtm.exe?Obitoscid.def>

CÉLULA DE SISTEMAS DE INFORMAÇÃO E ANÁLISE EM SAÚDE - CEINFA MORTALIDADE
GERAL EM FORTALEZA - SIM-Fortaleza-CE Cobertura: Total de Declarações de
Óbitos emitidas em Fortaleza atualizadas até às 8 horas do último dia
útil. Incluem óbitos ocorridos em Fortaleza e óbitos do interior
atestados pelo IML ou SVO. (Obs.: Dados sujeitos a revisão)

Parâmetros de consulta:

Linha: Bai Resid.(Alfab.) Coluna: Ano do Óbito Conteúdo: Óbitos Períodos
disponíveis: 2010 a 2020 Munic. Resid-CE: 230440 Fortaleza

#### Como são coletados os dados que alimentam o SIM

O documento básico e essencial à coleta de dados da mortalidade no
Brasil é a DECLARAÇÃO DE ÓBITO (DO) que, consequentemente, alimenta o
SIM.

A responsabilidade na emissão da DO é do médico, conforme prevê o artigo
115 do Código de Ética Médica, Artigo 1º da Resolução nº 1779/2005 do
Conselho Federal de Medicina e a Portaria SVS nº 116/2009. A DECLARAÇÃO
DE ÓBITO (DO) deve ser enviada aos Cartórios de Registro Civil para
liberação do sepultamento, bem como para a tomada de todas as medidas
legais em relação à morte.

A Declaração de Óbito é impressa e preenchida em três vias pré-numeradas
sequencialmente. Sua emissão e distribuição para os estados são de
competência exclusiva do Ministério da Saúde. A distribuição para os
municípios fica a cargo das Secretarias Estaduais de Saúde. Às
Secretarias Municipais de Saúde cabe o controle na distribuição das DO
entre os estabelecimentos de saúde, Institutos de Medicina Legal,
Serviços de Verificação de Óbitos, Cartórios do Registro Civil,
profissionais médicos e outras instituições que dela façam uso legal e
permitido. Compete às Secretarias de Saúde (Estado e Municípios) o
recolhimento das primeiras vias da Declaração de Óbito, junto aos
Estabelecimentos de Saúde e aos cartórios.

#### Como são obtidos e processados os dados do SIM

A mais recente publicação sobre coleta de dados, fluxo e periodicidade
de envio das informações acerca dos óbitos, e também dos nascidos vivos,
para os Sistemas de Informações geridos pela Secretaria de Vigilância à
Saúde é a Portaria SVS nº 116/2009, de 11/02/2009. Esta Portaria revoga
a Portaria SVS nº 20/2003.As DECLARAÇÕES DE ÓBITOS (DO) são preenchidas
pelas unidades notificantes do óbito (habitualmente no local de
ocorrência do óbito) e recolhidas, regularmente, pelas Secretarias
Municipais de Saúde. Nas Secretarias Municipais de Saúde (SMS), as
Declarações de Óbito são digitadas, processadas, criticadas e
consolidadas no SIM local. Em seguida, os dados informados pelos
municípios sobre mortalidade no nível local são transferidos à base de
dados do nível estadual que os agrega e envia-os ao nível federal. Tais
transferências são realizadas via WEB (internet) e ocorrem,
simultaneamente, nos três níveis de gestão. No nível federal, a SVS -
gestora do SIM - conta, na sua estrutura funcional, com a
Coordenação-Geral de Informações e Análises Epidemiológicas (CGIAE).
Subordinada ao Departamento de Análise de Saúde e Vigilância de Doenças
Não Transmissíveis (DASNT), a CGIAE trata da análise, avaliação e
distribuição das informações sobre o SIM, agregando-as por Estado, e
elaborando relatórios analíticos, painéis de indicadores e outros
instrumentos estatísticos de informações sobre mortalidade que são
disseminados para todo o país.

Mais detalhes são descritos na PORTARIA Nº 116, DE 11 DE FEVEREIRO DE
2009 que regulamenta a coleta de dados, fluxo e periodicidade de envio
das informações sobre óbitos e nascidos vivos para os Sistemas de
Informações em Saúde sob gestão da Secretaria de Vigilância em Saúde.

- <https://bvsms.saude.gov.br/bvs/saudelegis/svs/2009/prt0116_11_02_2009.html>

Manual de instruções para o preenchimento da declaração de nascido vivo:

- <https://bvsms.saude.gov.br/bvs/publicacoes/funasa/declaracao_nasc_vivo.pdf>

<hr>
<span id="fn.1">[1] Indicadores básicos para a saúde no Brasil: conceitos e aplicações
    / Rede Interagencial de Informação para a Saúde - Ripsa. -- 2. ed.
    -- Brasília: OrganizaçãoPan-Americana da Saúde, 2008. 349 p.: il.
    Disponível em:
    <http://tabnet.datasus.gov.br/tabdata/livroidb/2ed/indicadores.pdf>
<a href="#fnr.1">[Voltar]</a>
</span>
