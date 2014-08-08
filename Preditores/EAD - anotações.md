---
title: "Lista de tarefas"
author: "Henrique Pinto Gomide"
output:
  pdf_document:
    toc: yes
  html_document:
    toc: yes
  word_document: default
---


# Questões de pesquisa

## Módulo 4 - Projeto de Prevenção

1. Para quais alunos o programa de intervenção será destinado?
Serão apresentados através de opções fechadas, os atores da rede interna e externa da escola, indicando que os cursistas devem marcar os sujeitos para os quais seus projetos serão direcionados.
2. Qual será a metodologia utilizada para o desenvolvimento do programa?
Análise do conteúdo das respostas construídas pelos cursistas durante a realização da Atividade Colaborativa do Módulo 4.
3. Quais as possíveis barreiras para que o programa seja implementado na prática? Quais são os fatores que contribuem para a implementação do programa?

### Como fazer
Analisar as respostas construídas na atividade colaborativa do módulo 3 e fazer um apanhado geral dos pontos fortes e fracos da escola para apresentar na forma de um questionário estruturado ao final do módulo 4. 

## Módulo 5 - Implementação dos projetos

1. Os alunos para os quais o programa foi desenvolvido estão sendo atendidos? 
2. Qual a proporção dos sujeitos para os quais o projeto foi originalmente pensado foi realmente atendida?
3. Quais são as características dos sujeitos que foram contemplados pela projeto de prevenção e quais as características dos que não foram? 
4. A metodologia do projeto está sendo aplicada conforme o planejado? 
5. Quais são as dificuldades para implementação do programa? 

### Pós-avaliação (Seis meses após o término do curso):

6. O programa de intervenção continua a ser desenvolvido conforme o planejado?
7. Se não, foram feitas alterações e o programa está sendo aplicado de uma forma diferente ou ele não está sendo aplicado?

# Informações que precisamos coletar

Estas são as informações que precisamos coletar:

* Caracterização dos participantes do curso;
* ...

# Moodle

## Relatórios de atividades
O relatório de atividades oferece uma tabela com o nome do usuário e o número de vezes que participou de dadas atividades. Com o wget é possível importar todos as atividades e quem as fez.

Vantagens

* Podemos listar quais a frequência de uso de quase todos os componentes do curso. Inclusive os links.

Problemas

* O arquivo precisa ser importado e trabalhado. Será necessário gastar um tempo considerável (5 horas)
* A chave primária é o nome do usuário.

Export files 

```{r}
wget --load-cookies=cookies.txt "http://www.projetosenad.ufjf.br/report/participation/index.php?id=3&roleid=5&instanceid=11&timefrom=0&action=&perpage=20&perpage=10000"
```


## Andamento das atividades

É possível importar um arquivo csv com o status da atividade. São listadas 17 atividades. Olhar com detalhes para saber quais atividades realmente contam pontos.

Vantagens

* O arquivo está bem estruturado e precisa de pouco processamento para realizar as atividades,
* Nem todas as atividades estão incluídas. Confererir se alguma importante foi esquecida.


## Atividade do curso

É uma lista processada dos logs com o número de visualização de cada atividade do curso. É possível exportar algumas das atividades e principalmente a pontuação de cada questionário.

Desvantagem

* A lista não descreve muita informação. 

## Notas

É uma lista de notas para cada aluno. Pode-se gerar uma visualização com todos alunos. É importante conferir as diferenças com a atividade _Andamento das atividades_.

Vantagens

* Contém as notas de todos os alunos

Desvantagens

* Requer processamento dado a natureza dos arquivos.


# Ideias

1. Artigo descrito sobre o curso e suas características básicas. Tentar identificar perfis dos alunos.
2. Avaliar o conhecimento dos alunos e criar um questionário validado por TRI para avaliar o curso.
3. Tentar estabelecer modelos de previsão de abandono usando aprendizagem por máquinas e mineração de dados.


