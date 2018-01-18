#!/bin/bash
set -eux -o pipefail
# Скрипт для для обновления сайта на GitHub Pages.

echo "Собираем новую версию сайта..."
./just_build.sh

# запоминаем сообщение текущей ревизии
commit_message="$(git log --format=%B HEAD^1..HEAD)"

(
    cd _site
    git init
    git remote add source ..
    git fetch source

    echo "Создаём коммит из _site..."
    export GIT_AUTHOR_EMAIL=root@localhost
    export GIT_COMMITTER_EMAIL=root@localhost
    git add .
    git write-tree                          | xargs \
    git commit-tree                                 \
        -p source/master                            \
        -m "$commit_message" `: tree id :`  | xargs \
    git checkout --quiet `: commit id :`

    echo "Копируем коммит в оригинальный репозиторий..."
    git push --force source HEAD:refs/heads/gh-pages
)

echo "Публикуем gh-pages на сервере..."
git push --force origin gh-pages:gh-pages

echo "Готово!"
