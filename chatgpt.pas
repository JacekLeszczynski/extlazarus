unit ChatGPT;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, OpenAIClient, OpenAIDtos;

type
  TChatGPTOnStart = procedure(aText: string) of object;
  TChatGPTOnStop = procedure(aText: string; aError: boolean) of object;

  { TChatGPT }

  TChatGPT = class(TComponent)
  private
    FApiKey: string;
    FFrequency: double;
    FFrequencyBool: boolean;
    FIsInit: boolean;
    FMaxTokens: integer;
    FModel: string;
    FOnStart: TChatGPTOnStart;
    FOnStop: TChatGPTOnStop;
    FPresence: double;
    FPresenceBool: boolean;
    FTemperature: double;
    FTemperatureBool: boolean;
    FThreadExec: boolean;
    FTopP: double;
    FTopPBool: boolean;
    chatgptclient: IOpenAIClient;
    procedure Init;
    procedure SetApiKey(AValue: string);
    procedure SetTemperature(AValue: double);
    procedure SetTopP(AValue: double);
    function AskQuestion(const aQuestion: string): string;
    procedure _OnStart(aText: string);
    procedure _OnStop(aText: string; aError: boolean);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute(const aQuestion: string);
  published
    property IsInit: boolean read FIsInit;
    //Wykonuj polecenia w wątku
    property ThreadExec: boolean read FThreadExec write FThreadExec default false;
    //Unikalny API KEY
    property ApiKey: string read FApiKey write SetApiKey;
    //Model Language
    property Model: string read FModel write FModel;
    //The maximum number of [tokens](/tokenizer) to generate in the completion.
    //The token count of your prompt plus `max_tokens` cannot exceed the model's context length.
    //Most models have a context length of 2048 tokens (except for the newest models, which support 4096).
    property MaxTokens: integer read FMaxTokens write FMaxTokens default 2048;
    // What [sampling temperature](https://towardsdatascience.com/how-to-sample-from-language-models-682bceb97277) to use.
    //Higher values means the model will take more risks.
    //Try 0.9 for more creative applications, and 0 (argmax sampling) for ones with a well-defined answer.
    // We generally recommend altering this or `top_p` but not both.
    property Temperature: double read FTemperature write SetTemperature;
    property TemperatureBool: boolean read FTemperatureBool write FTemperatureBool;
    //An alternative to sampling with temperature, called nucleus sampling,
    //where the model considers the results of the tokens with top_p probability mass.
    //So 0.1 means only the tokens comprising the top 10% probability mass are considered.
    //We generally recommend altering this or `temperature` but not both.
    property TopP: double read FTopP write SetTopP;
    property TopPBool: boolean read FTopPBool write FTopPBool;
    //Number between -2.0 and 2.0.
    //Positive values penalize new tokens based on their existing frequency in the text so far,
    //decreasing the model's likelihood to repeat the same line verbatim.
    //[See more information about frequency and presence penalties.](/docs/api-reference/parameter-details)
    property FrequencyPenalty: double read FFrequency write FFrequency;
    property FrequencyPenaltyBool: boolean read FFrequencyBool write FFrequencyBool;
    //Number between -2.0 and 2.0.
    //Positive values penalize new tokens based on whether they appear in the text so far,
    //increasing the model's likelihood to talk about new topics.
    //[See more information about frequency and presence penalties.](/docs/api-reference/parameter-details)
    property PresencePenalty: double read FPresence write FPresence;
    property PresencePenaltyBool: boolean read FPresenceBool write FPresenceBool;
    property OnStart: TChatGPTOnStart read FOnStart write FOnStart;
    property OnStop: TChatGPTOnStop read FOnStop write FOnStop;
  end;

  { TChatGPTWatek }

  TChatGPTWatek = class(TThread)
  private
    FOnStart: TChatGPTOnStart;
    FOnStop: TChatGPTOnStop;
    text: string;
    error: boolean;
    chatgptclient: IOpenAIClient;
    vQuestion,vModel: string;
    vMaxTokens: integer;
    vTemperatureBool,vTopPBool,vFrequencyBool,vPresenceBool: boolean;
    vTemperature,vTopP,vFrequency,vPresence: double;
    procedure _start;
    procedure _stop;
    function AskQuestion: string;
  public
    constructor Create(const aQuestion,aApiKey,aModel: string; const aMaxTokens: integer; aTemperatureBool: boolean; aTemperature: double; aTopPBool: boolean; aTopP: double; aFrequencyBool: boolean; aFrequency: double; aPresenceBool: boolean; aPresence: double);
    destructor Destroy; override;
    procedure Execute; override;
  published
    property OnStart: TChatGPTOnStart read FOnStart write FOnStart;
    property OnStop: TChatGPTOnStop read FOnStop write FOnStop;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I chatgpt_icon.lrs}
  RegisterComponents('lNet',[TChatGPT]);
end;

{ TChatGPTWatek }

procedure TChatGPTWatek._start;
begin
  if assigned(FOnStart) then FOnStart(vQuestion);
end;

procedure TChatGPTWatek._stop;
begin
  if assigned(FOnStop) then FOnStop(text,error);
end;

function TChatGPTWatek.AskQuestion: string;
var
  Request: TCreateCompletionRequest;
  Response: TCreateCompletionResponse;
begin
  Response:=nil;
  Request:=TCreateCompletionRequest.Create;
  try
    Request.Prompt:=vQuestion;
    Request.Model:=vModel;
    Request.MaxTokens:=vMaxTokens;
    if vTemperatureBool then Request.Temperature:=vTemperature;
    if vTopPBool then Request.TopP:=vTopP;
    if vFrequencyBool then Request.FrequencyPenalty:=vFrequency;
    if vPresenceBool then Request.PresencePenalty:=vPresence;
    Response:=chatgptclient.OpenAI.CreateCompletion(Request);
    if Assigned(Response.Choices) and (Response.Choices.Count>0) then result:=Response.Choices[0].Text else result:='';
  finally
    Request.Free;
    Response.Free;
  end;
end;

constructor TChatGPTWatek.Create(const aQuestion, aApiKey, aModel: string;
  const aMaxTokens: integer; aTemperatureBool: boolean; aTemperature: double;
  aTopPBool: boolean; aTopP: double; aFrequencyBool: boolean;
  aFrequency: double; aPresenceBool: boolean; aPresence: double);
begin
  inherited Create(true);
  FreeOnTerminate:=true;
  chatgptclient:=TOpenAIClient.Create;
  chatgptclient.Config.AccessToken:=aApiKey;
  vQuestion:=aQuestion;
  vModel:=aModel;
  vMaxTokens:=aMaxTokens;
  vTemperatureBool:=aTemperatureBool;
  vTemperature:=aTemperature;
  vTopPBool:=aTopPBool;
  vTopP:=aTopP;
  vFrequencyBool:=aFrequencyBool;
  vFrequency:=aFrequency;
  vPresenceBool:=aPresenceBool;
  vPresence:=aPresence;
end;

destructor TChatGPTWatek.Destroy;
begin
  inherited Destroy;
end;

procedure TChatGPTWatek.Execute;
begin
  synchronize(@_start);
  try
    try
      text:=AskQuestion;
      error:=false;
    except
      on E: Exception do
      begin
        text:=E.Message;
        error:=true;
      end;
    end;
  finally
    synchronize(@_stop);
  end;
end;

{ TChatGPT }

procedure TChatGPT.Init;
begin
  chatgptclient:=TOpenAIClient.Create;
  chatgptclient.Config.AccessToken:=FApiKey;
  FIsInit:=true;
end;

procedure TChatGPT.SetApiKey(AValue: string);
begin
  if FApiKey=AValue then Exit;
  FApiKey:=AValue;
  if fIsInit then chatgptclient.Config.AccessToken:=FApiKey;
end;

procedure TChatGPT.SetTemperature(AValue: double);
begin
  if FTemperature=AValue then Exit;
  FTemperatureBool:=true;
  FTopPBool:=false;
  FTemperature:=AValue;
end;

procedure TChatGPT.SetTopP(AValue: double);
begin
  if FTopP=AValue then Exit;
  FTemperatureBool:=false;
  FTopPBool:=true;
  FTopP:=AValue;
end;

function TChatGPT.AskQuestion(const aQuestion: string): string;
var
  Request: TCreateCompletionRequest;
  Response: TCreateCompletionResponse;
begin
  Response:=nil;
  Request:=TCreateCompletionRequest.Create;
  try
    Request.Prompt:=aQuestion;
    Request.Model:=FModel;
    Request.MaxTokens:=FMaxTokens;
    if FTemperatureBool then Request.Temperature:=FTemperature;
    if FTopPBool then Request.TopP:=FTopP;
    if FFrequencyBool then Request.FrequencyPenalty:=FFrequency;
    if FPresenceBool then Request.PresencePenalty:=FPresence;
    Response:=chatgptclient.OpenAI.CreateCompletion(Request);
    if Assigned(Response.Choices) and (Response.Choices.Count>0) then result:=Response.Choices[0].Text else result:='';
  finally
    Request.Free;
    Response.Free;
  end;
end;

procedure TChatGPT._OnStart(aText: string);
begin
  if assigned(FOnStart) then FOnStart(aText);
end;

procedure TChatGPT._OnStop(aText: string; aError: boolean);
begin
  if assigned(FOnStop) then FOnStop(aText,aError);
end;

constructor TChatGPT.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIsInit:=false;
  FThreadExec:=false;
  FApiKey:='';
  FModel:='';
  FMaxTokens:=2048;
  FTemperature:=1.0;
  FTemperatureBool:=false;
  FTopP:=1.0;
  FTopPBool:=false;
  FFrequency:=0;
  FFrequencyBool:=false;
  FPresence:=0;
  FPresenceBool:=false;
end;

destructor TChatGPT.Destroy;
begin
  inherited Destroy;
end;

procedure TChatGPT.Execute(const aQuestion: string);
var
  s: string;
  error: boolean;
  w: TChatGPTWatek;
begin
  if FThreadExec then
  begin
    w:=TChatGPTWatek.Create(aQuestion,FApiKey,FModel,FMaxTokens,FTemperatureBool,FTemperature,FTopPBool,FTopP,FFrequencyBool,FFrequency,FPresenceBool,FPresence);
    w.OnStart:=@_OnStart;
    w.OnStop:=@_OnStop;
    w.Start;
  end else begin
    if assigned(FOnStart) then FOnStart(aQuestion);
    try
      if not FIsInit then Init;
      s:=AskQuestion(aQuestion);
      error:=false;
    except
      on E: Exception do
      begin
        s:=E.Message;
        error:=true;
      end;
    end;
    if assigned(FOnStop) then FOnStop(s,error);
  end;
end;

end.
