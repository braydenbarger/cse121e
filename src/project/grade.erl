-module(grade).
-export([start/0, grade_to_letter/1, grade_sign/1, grade_message/1]).

grade_to_letter(Grade) when Grade >= 90 -> {a, ""};
grade_to_letter(Grade) when Grade >= 80 -> {b, ""};
grade_to_letter(Grade) when Grade >= 70 -> {c, ""};
grade_to_letter(Grade) when Grade >= 60 -> {d, ""};
grade_to_letter(Grade) when Grade < 60 -> {f, ""}.

grade_sign(Grade) when Grade >= 93 -> "";
grade_sign(Grade) when Grade rem 10 >= 7 -> "+"; 
grade_sign(Grade) when Grade rem 10 < 3 -> "-";
grade_sign(_) -> "".

grade_message(Grade) ->
    {Letter, Sign} = grade_to_letter(Grade),
    FinalSign = grade_sign(Grade),
    io:format("Your letter grade is ~s~s~n", [Letter, FinalSign]),
    case Grade >= 70 of
        true -> io:format("Congratulations, you passed the class!~n");
        false -> io:format("Better luck next time.~n")
    end.

start() ->
    io:format("What is your grade?: "),
    {ok, [Grade]} = io:fread("", "~d"),
    grade_message(Grade).
