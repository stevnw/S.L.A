# S.L.A
S.L.A - Flashcard and Quiz tool for language learning. S.L.A has 3 main modes - "learn", "match" and "targeted". Learn is like flashcards, where you go through the deck. Match is a game where you have to match the correct meaning to the word/phrase. Targeted lets you select a range from your deck and then use it in either "learn" or "match" modes.

<img width="800" height="604" alt="image" src="https://github.com/user-attachments/assets/cf8dda52-51b6-42cc-bed1-a4510c5e7ecc" />

<img width="800" height="604" alt="image" src="https://github.com/user-attachments/assets/6367c0db-b556-4511-8331-915828a73be7" />

<img width="800" height="604" alt="image" src="https://github.com/user-attachments/assets/d1add805-59a8-4f0c-be74-4fd419b816c2" />

## Note:

This program features 5 display themes: light, dark, sepia, solarized and sakura. 

This program is provided as is. <i><b>I have not included any datasets with it.</i></b> It makes uses .tsv files in the format:

<pre> Language_Word/Phrase  Meaning  Readings*  Sound </pre>

<sub>* leave Readings blank if the languages doesn't need them - this is more for Japanese or Chinese with kana and pinyin readings </sub>

All audio has to be .wav. A valid file path can be something like:

<pre> audio/00001.wav </pre>

e.g.

<pre> Hej / Hallo	Hello		audio/00002.wav </pre>

File structure looks like:

<pre> audio (folder)  config.txt  SLA-1.1-standalone.jar  Xefjord_Danish.tsv </pre>


Fonts:

Create a folder in the root directory called "user" - just place the fonts (.ttf or .otf) here. 

The font will then appear in the options if configured correctly.

____

My intended use for this is as an alternative to Anki for sorted data from Anki decks, e.g. [Xefjord's Complete Language Series](https://xefjord.wixsite.com/xefscompletelangs/courses) 

Check out my Anki Add-on for a quick way to pull data sets from Anki: https://github.com/stevnw/ExportWithAudio-Anki

<sub>S.L.A stands for Swing Language Application - in case you were wondering hehe</sub>
