wget -i raw-data.urls.txt

for file in *.zip; do
    unzip "$file" && rm "$file"
done

