export CUDA_VISIBLE_DEVICES=0

python run_bert.py \
--model_type bert \
--model_name_or_path chinese_roberta \
--do_train \
--do_eval \
--do_test \
--data_dir ./data/data \
--output_dir ./model_roberta \
--max_seq_length 128 \
--split_num 1 \
--lstm_hidden_size 512 \
--lstm_layers 1 \
--lstm_dropout 0.1 \
--eval_steps 100 \
--per_gpu_train_batch_size 32 \
--gradient_accumulation_steps 4 \
--warmup_steps 0 \
--per_gpu_eval_batch_size 64 \
--learning_rate 1e-5 \
--adam_epsilon 1e-6 \
--weight_decay 0 \
--train_steps 20000





