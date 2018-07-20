austin_text <- all_Austin %>%
  group_by(book) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  mutate(writer = "Austin")

dickens_text <- all_dickens %>%
  group_by(book) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  mutate(writer = "Dickens") %>% 
  select(-gutenberg_id)

together_text <- rbind(austin_text, dickens_text)

######### We will use Oliver Twist and Sense and Sensibility books for our test data

## Train Data
train_text <- together_text %>% filter(book != "Sense & Sensibility", book !="Oliver Twist")

### Unigrams
train_unigrams <- train_text %>%
  unnest_tokens(word, text, token = "ngrams", n = 1)

train_unigrams_filtered <- train_unigrams %>%
  filter(!word %in% stop_words$word)

train_top_unigrams <- train_unigrams_filtered %>% 
  count(word, sort = TRUE) %>% 
  rename(count = n) %>% 
  top_n(wt = count, n = 20)

train_unigrams_counts <- train_unigrams_filtered %>% 
  filter(word %in% train_top_unigrams$word) %>% 
  group_by(book, chapter, writer) %>% 
  count(word, sort = TRUE) %>% 
  rename(count = n) %>% 
  spread(word, count) %>%
  ungroup() %>% 
  unite(document, book, chapter, writer)
train_unigrams_counts[is.na(train_unigrams_counts)] <- 0

### Bigrams

train_bigrams <- train_text %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

train_bigrams_separated <- train_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

train_bigrams_filtered <- train_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

train_bigrams_united <- train_bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

train_top_bigrams <- train_bigrams_united %>% 
  count(bigram, sort = TRUE) %>% 
  rename(count = n) %>% 
  top_n(wt = count, n = 10)

train_bigrams_counts <- train_bigrams_united%>% 
  filter(bigram %in% train_top_bigrams$bigram) %>% 
  group_by(book, chapter, writer) %>% 
  count(bigram, sort = TRUE) %>% 
  rename(count = n) %>% 
  spread(bigram, count) %>%
  ungroup() %>% 
  unite(document, book, chapter, writer)

train_bigrams_counts[is.na(train_bigrams_counts)] <- 0

train_data <- full_join(train_unigrams_counts, train_bigrams_counts, by = "document")
train_data[is.na(train_data)] <- 0
train_data <- train_data %>%
  separate(document, c("book", "chapter", "writer"), sep = "_") %>% 
  mutate(writer =1 * (writer == ("Dickens")))

####  model
model <- glm(
  writer~.,
  data = train_data %>% select(-book,-chapter,-house,-looked,-`miss tox`,
                               -`miss havisham`,-`captain cuttle`,-`sir leicester`,
                                -`sir mulberry`,-`sir returned`,-mind,-dear,
                               -`sir replied`,-replied,-`dear sir`,-sir,-door,-`sir thomas`,-eyes),
  family = binomial)
summary(model)
## Test Data
test_text <- together_text %>% filter(book == "Sense & Sensibility" | book =="Oliver Twist")

### Unigrams
test_unigrams <- test_text %>%
  unnest_tokens(word, text, token = "ngrams", n = 1)

test_unigrams_filtered <- test_unigrams %>%
  filter(!word %in% stop_words$word)

test_unigrams_counts <- test_unigrams_filtered %>% 
  filter(word %in% train_top_unigrams$word) %>% 
  group_by(book, chapter, writer) %>% 
  count(word, sort = TRUE) %>% 
  rename(count = n) %>% 
  spread(word, count) %>%
  ungroup() %>% 
  unite(document, book, chapter, writer)
test_unigrams_counts[is.na(test_unigrams_counts)] <- 0

### Bigrams

test_bigrams <- test_text %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

test_bigrams_separated <- test_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

test_bigrams_filtered <- test_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

test_bigrams_united <- test_bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

test_bigrams_counts <- test_bigrams_united%>% 
  filter(bigram %in% train_top_bigrams$bigram) %>% 
  group_by(book, chapter, writer) %>% 
  count(bigram, sort = TRUE) %>% 
  rename(count = n) %>% 
  spread(bigram, count) %>%
  ungroup() %>% 
  unite(document, book, chapter, writer)

test_bigrams_counts[is.na(test_bigrams_counts)] <- 0

test_data <- full_join(test_unigrams_counts, test_bigrams_counts, by = "document")
test_data[is.na(test_data)] <- 0
test_data <- test_data %>%  
  separate(document, c("book", "chapter", "writer"), sep = "_") %>% 
  mutate(writer = 1 * (writer == ("Dickens")))

train_data$prediction = predict(model, newdata = train_data , type = "response")

#setting cutoff
cost_fp = 100;cost_fn = 100
roc_info = ROCInfo( data = train_data, predict = "prediction", 
                    actual = "writer", cost.fp = cost_fp, cost.fn = cost_fn )
roc_info$cutoff -> co

P <- train_data %>% filter(prediction > 0.5) %>% count()
N <- train_data %>% filter(prediction <= co) %>% count()
TP <- train_data %>% filter(prediction > co,writer == 1) %>% count()
TN <- train_data %>% filter(prediction <= co, writer == 0) %>% count()
FP <- train_data %>% filter(prediction > co,writer== 0) %>% count()
FN <- train_data %>% filter(prediction <= co, writer == 1) %>% count()
ACC <- (TP + TN)/ (P + N)
sprintf("Accuracy (ACC): %.3f", ACC)
sprintf("Error (ERR): %.3f%%", (1 - ACC)* 100)

test_data$prediction = predict(model, newdata = test_data , type = "response")
cm_info = ConfusionMatrixInfo( data = test_data, predict = "prediction", 
                               actual = "writer", cutoff = co)

cm_info$plot


P <- test_data %>% filter(prediction > co) %>% count()
N <- test_data %>% filter(prediction <= co) %>% count()
TP <- test_data %>% filter(prediction > co,writer == 1) %>% count()
TN <- test_data %>% filter(prediction <= co, writer == 0) %>% count()
FP <- test_data %>% filter(prediction > co,writer== 0) %>% count()
FN <- test_data %>% filter(prediction <= co, writer == 1) %>% count()
ACC <- (TP + TN)/ (P + N)
sprintf("Accuracy (ACC): %.3f", ACC)
sprintf("Error (ERR): %.3f%%", (1 - ACC)* 100)
