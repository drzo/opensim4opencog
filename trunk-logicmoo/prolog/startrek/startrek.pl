
:-catch(guitracer,E,writeq(E)),nl.
:-set_prolog_flag(double_quotes,string).

%% pipequoted(";(find-or-create-constant \"OSimCurrentStateMt\")").
%% pipequoted(";(osim-assert '(#$isa *WorldStaticStateMt* #$DataMicrotheory) *WORLDVOCABMT*)").
'find-or-create-constant'(string("BoundsOfDirectionFn")).

%% pipequoted("load file '10.kif'").
%% pipequoted("load file '10.kif.txt'").
%% pipequoted("load file '10.mob'").
%% pipequoted("; Sourcing Mob File C:OpenSimsrcdaxmooworlds10.mob").
%% pipequoted("; mob #1000").
'find-or-create-constant'(string("NpcCol1000")).

:-osimAssert(isa('NpcCol1000','BPVAgentType'),'*WorldVocabularyMt*').

:-osimAssert(genls('NpcCol1000','Agent-Generic'),'*WorldVocabularyMt*').

:-osimAssert(relationAllInstance(nicknames,'NpcCol1000',string("Lieutenant")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'NpcCol1000',string("Commander")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'NpcCol1000',string("Geordi")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'NpcCol1000',string("LaForge")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nameString,'NpcCol1000',string("Geordi LaForge")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(definiteDescriptions,'NpcCol1000',string("Lieutenant Commander Geordi LaForge is standing here")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(personalIndentifyingCharacteristic,'NpcCol1000',string("Geordi is the Chief Engineer of the Enterprise")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(personalIndentifyingCharacteristic,'NpcCol1000',string("He's blind, so he wears a special VISOR that lets him see things")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1000',string("ACT_STAY_ZONE")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1000',string("MEMORY")),'*WorldStaticStateMt*').

:-osimAssert(frequencyOfActionType('RevengeAction','NpcCol1000',bodilyDoer,'Often'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1000',string("HELPER")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1000',string("ACT_FRIEND")),'*WorldStaticStateMt*').

:-osimAssert('typeBehaviorCapable-PerformedBy'('NpcCol1000','HelpingAnAgent'),'*WorldStaticStateMt*').

:-osimAssert(frequencyOfActionType('ProtectingSomething','NpcCol1000',bodilyDoer,'Often'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1000',string("NOCHARM")),'*WorldStaticStateMt*').

:-osimAssert(typeBehaviorIncapable('NpcCol1000','InfluencingAnAgent',recipientOfInfo),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1000',string("NOBASH")),'*WorldStaticStateMt*').

:-osimAssert(typeBehaviorIncapable('NpcCol1000','IncurringPhysicalDamage',damages),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1000',string("NOBLIND")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1000',string("NPC_DETECT_INVIS")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1000',string("NPC_NOTRACK")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(moralCharacter,'NpcCol1000',['MediumAmountFn','Goodness-Moral']),'*WorldStaticStateMt*').

%% pipequoted(";(osim-assert '(#$relationAllInstance #$skillLevel #$NpcCol1000 #$SingleDoerAction #$Effectiveness #$veryHighAmountOf) *WorldStaticStateMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )").
:-osimAssert(relationAllInstance(comment,'NpcCol1000',string("+mudToHitArmorClass0: 3")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(toughnessOfObject,'NpcCol1000','HighAmountFn'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1000',string("mudMaxHitPoints: 12d12+3200")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1000',string("#$PunchingSomething mudBareHandDamage: 9d9+42")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(netWorth,'NpcCol1000',['USDollarFn',75000]),'*WorldStaticStateMt*').

%% pipequoted(";(osim-assert '(#$relationAllInstance #$skillLevel #$NpcCol1000 #$IntentionalMentalEvent #$Effectiveness #$mediumAmountOf) *WorldStaticStateMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )").
:-osimAssert(possible(relationAllInstance(postureOfAnimal,'NpcCol1000','UprightPosture')),'*WorldStaticStateMt*').

:-osimAssert(genls('NpcCol1000','MaleAnimal'),'*WorldStaticStateMt*').

%% pipequoted("; mob").
%% pipequoted("; mob #1001").

'find-or-create-constant'(string("NpcCol1001")).

:-osimAssert(isa('NpcCol1001','BPVAgentType'),'*WorldVocabularyMt*').

:-osimAssert(genls('NpcCol1001','Agent-Generic'),'*WorldVocabularyMt*').

:-osimAssert(relationAllInstance(nicknames,'NpcCol1001',string("Lieutenant")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'NpcCol1001',string("Commander")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'NpcCol1001',string("Data")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'NpcCol1001',string("Android")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nameString,'NpcCol1001',string("Data")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(definiteDescriptions,'NpcCol1001',string("Lieutenant Commander Data is here, trying to be more human")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(personalIndentifyingCharacteristic,'NpcCol1001',string("Data is the only android on the Enterprise, and the only android in all of Starfleet")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(personalIndentifyingCharacteristic,'NpcCol1001',string("He possesses super-human strength, and is extremely tough")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1001',string("ACT_NICE_THIEF")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1001',string("AWARE")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1001',string("NOBACKSTAB")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1001',string("ACT_STAY_ZONE")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1001',string("MEMORY")),'*WorldStaticStateMt*').

:-osimAssert(frequencyOfActionType('RevengeAction','NpcCol1001',bodilyDoer,'Often'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1001',string("HELPER")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1001',string("ACT_FRIEND")),'*WorldStaticStateMt*').

:-osimAssert('typeBehaviorCapable-PerformedBy'('NpcCol1001','HelpingAnAgent'),'*WorldStaticStateMt*').

:-osimAssert(frequencyOfActionType('ProtectingSomething','NpcCol1001',bodilyDoer,'Often'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1001',string("NOCHARM")),'*WorldStaticStateMt*').

:-osimAssert(typeBehaviorIncapable('NpcCol1001','InfluencingAnAgent',recipientOfInfo),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1001',string("NOSUMMON")),'*WorldStaticStateMt*').

:-osimAssert(typeBehaviorIncapable('NpcCol1001','CoercingAnAgentToMove',objectMoving),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1001',string("NOSLEEP")),'*WorldStaticStateMt*').

:-osimAssert(typeBehaviorIncapable('NpcCol1001','Sleeping',bodilyDoer),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1001',string("NOBASH")),'*WorldStaticStateMt*').

:-osimAssert(typeBehaviorIncapable('NpcCol1001','IncurringPhysicalDamage',damages),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1001',string("NOBLIND")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1001',string("NPC_DETECT_INVIS")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1001',string("NPC_NOTRACK")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(moralCharacter,'NpcCol1001',['MediumAmountFn','Goodness-Moral']),'*WorldStaticStateMt*').

%% pipequoted(";(osim-assert '(#$relationAllInstance #$skillLevel #$NpcCol1001 #$SingleDoerAction #$Effectiveness #$veryHighAmountOf) *WorldStaticStateMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )").
:-osimAssert(relationAllInstance(comment,'NpcCol1001',string("+mudToHitArmorClass0: 1")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(toughnessOfObject,'NpcCol1001','VeryHighAmountFn'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1001',string("mudMaxHitPoints: 18d18+4000")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1001',string("#$PunchingSomething mudBareHandDamage: 10d10+75")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(netWorth,'NpcCol1001',['USDollarFn',125000]),'*WorldStaticStateMt*').

%% pipequoted(";(osim-assert '(#$relationAllInstance #$skillLevel #$NpcCol1001 #$IntentionalMentalEvent #$Effectiveness #$veryHighAmountOf) *WorldStaticStateMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )").
:-osimAssert(possible(relationAllInstance(postureOfAnimal,'NpcCol1001','UprightPosture')),'*WorldStaticStateMt*').

:-osimAssert(genls('NpcCol1001','MaleAnimal'),'*WorldStaticStateMt*').



:-osimAssert(isa(['OSimClassFn',string("player_osimmarine_mp")],'BPVAgentType'),'*WorldVocabularyMt*').

:-osimAssert(genls(['OSimClassFn',string("player_osimmarine_mp")],'Agent-Generic'),'*WorldVocabularyMt*').

:-osimAssert(relationAllInstance(nicknames,['OSimClassFn',string("player_osimmarine_mp")],string("Lieutenant")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,['OSimClassFn',string("player_osimmarine_mp")],string("Commander")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,['OSimClassFn',string("player_osimmarine_mp")],string("Human")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nameString,['OSimClassFn',string("player_osimmarine_mp")],string("Player")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(definiteDescriptions,['OSimClassFn',string("player_osimmarine_mp")],string("Lieutenant Commander Player is here, trying to be more human")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(personalIndentifyingCharacteristic,['OSimClassFn',string("player_osimmarine_mp")],string("Player is the only Player on the Enterprise, and the only in all of Starfleet")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(personalIndentifyingCharacteristic,['OSimClassFn',string("player_osimmarine_mp")],string("He possesses super-human strength, and is extremely tough")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,['OSimClassFn',string("player_osimmarine_mp")],string("ACT_NICE_THIEF")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,['OSimClassFn',string("player_osimmarine_mp")],string("AWARE")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,['OSimClassFn',string("player_osimmarine_mp")],string("NOBACKSTAB")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,['OSimClassFn',string("player_osimmarine_mp")],string("ACT_STAY_ZONE")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,['OSimClassFn',string("player_osimmarine_mp")],string("MEMORY")),'*WorldStaticStateMt*').

:-osimAssert(frequencyOfActionType('RevengeAction',['OSimClassFn',string("player_osimmarine_mp")],bodilyDoer,'Often'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,['OSimClassFn',string("player_osimmarine_mp")],string("HELPER")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,['OSimClassFn',string("player_osimmarine_mp")],string("ACT_FRIEND")),'*WorldStaticStateMt*').

:-osimAssert('typeBehaviorCapable-PerformedBy'(['OSimClassFn',string("player_osimmarine_mp")],'HelpingAnAgent'),'*WorldStaticStateMt*').

:-osimAssert(frequencyOfActionType('ProtectingSomething',['OSimClassFn',string("player_osimmarine_mp")],bodilyDoer,'Often'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,['OSimClassFn',string("player_osimmarine_mp")],string("NOCHARM")),'*WorldStaticStateMt*').

:-osimAssert(typeBehaviorIncapable(['OSimClassFn',string("player_osimmarine_mp")],'InfluencingAnAgent',recipientOfInfo),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,['OSimClassFn',string("player_osimmarine_mp")],string("NOSUMMON")),'*WorldStaticStateMt*').

:-osimAssert(typeBehaviorIncapable(['OSimClassFn',string("player_osimmarine_mp")],'CoercingAnAgentToMove',objectMoving),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,['OSimClassFn',string("player_osimmarine_mp")],string("NOSLEEP")),'*WorldStaticStateMt*').

:-osimAssert(typeBehaviorIncapable(['OSimClassFn',string("player_osimmarine_mp")],'Sleeping',bodilyDoer),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,['OSimClassFn',string("player_osimmarine_mp")],string("NOBASH")),'*WorldStaticStateMt*').

:-osimAssert(typeBehaviorIncapable(['OSimClassFn',string("player_osimmarine_mp")],'IncurringPhysicalDamage',damages),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,['OSimClassFn',string("player_osimmarine_mp")],string("NOBLIND")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,['OSimClassFn',string("player_osimmarine_mp")],string("NPC_DETECT_INVIS")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,['OSimClassFn',string("player_osimmarine_mp")],string("NPC_NOTRACK")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(moralCharacter,['OSimClassFn',string("player_osimmarine_mp")],['MediumAmountFn','Goodness-Moral']),'*WorldStaticStateMt*').

%% pipequoted(";(osim-assert '(#$relationAllInstance #$skillLevel (#$OSimClassFn \"player_osimmarine_mp\") #$SingleDoerAction #$Effectiveness #$veryHighAmountOf) *WorldStaticStateMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )").
:-osimAssert(relationAllInstance(comment,['OSimClassFn',string("player_osimmarine_mp")],string("+mudToHitArmorClass0: 1")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(toughnessOfObject,['OSimClassFn',string("player_osimmarine_mp")],'VeryHighAmountFn'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,['OSimClassFn',string("player_osimmarine_mp")],string("mudMaxHitPoints: 18d18+4000")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,['OSimClassFn',string("player_osimmarine_mp")],string("#$PunchingSomething mudBareHandDamage: 10d10+75")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(netWorth,['OSimClassFn',string("player_osimmarine_mp")],['USDollarFn',125000]),'*WorldStaticStateMt*').

%% pipequoted(";(osim-assert '(#$relationAllInstance #$skillLevel (#$OSimClassFn \"player_osimmarine_mp\") #$IntentionalMentalEvent #$Effectiveness #$veryHighAmountOf) *WorldStaticStateMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )").
:-osimAssert(possible(relationAllInstance(postureOfAnimal,['OSimClassFn',string("player_osimmarine_mp")],'UprightPosture')),'*WorldStaticStateMt*').

:-osimAssert(genls(['OSimClassFn',string("player_osimmarine_mp")],'MaleAnimal'),'*WorldStaticStateMt*').

%% pipequoted("; mob").
%% pipequoted("; mob #1002").
'find-or-create-constant'(string("NpcCol1002")).

:-osimAssert(isa('NpcCol1002','BPVAgentType'),'*WorldVocabularyMt*').

:-osimAssert(genls('NpcCol1002','Agent-Generic'),'*WorldVocabularyMt*').

:-osimAssert(relationAllInstance(nicknames,'NpcCol1002',string("Lieutenant")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'NpcCol1002',string("Worf")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'NpcCol1002',string("Klingon")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nameString,'NpcCol1002',string("Lieutenant Worf")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(definiteDescriptions,'NpcCol1002',string("Lieutenant Worf is here, looking pretty mean")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(personalIndentifyingCharacteristic,'NpcCol1002',string("Worf is the first Klingon to have joined Starfleet")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(personalIndentifyingCharacteristic,'NpcCol1002',string("He's Chief of Security of the Enterprise, and he's plenty strong")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1002',string("ACT_STAY_ZONE")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1002',string("MEMORY")),'*WorldStaticStateMt*').

:-osimAssert(frequencyOfActionType('RevengeAction','NpcCol1002',bodilyDoer,'Often'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1002',string("HELPER")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1002',string("ACT_FRIEND")),'*WorldStaticStateMt*').

:-osimAssert('typeBehaviorCapable-PerformedBy'('NpcCol1002','HelpingAnAgent'),'*WorldStaticStateMt*').

:-osimAssert(frequencyOfActionType('ProtectingSomething','NpcCol1002',bodilyDoer,'Often'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1002',string("NOCHARM")),'*WorldStaticStateMt*').

:-osimAssert(typeBehaviorIncapable('NpcCol1002','InfluencingAnAgent',recipientOfInfo),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1002',string("NOBASH")),'*WorldStaticStateMt*').

:-osimAssert(typeBehaviorIncapable('NpcCol1002','IncurringPhysicalDamage',damages),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1002',string("NOBLIND")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1002',string("NPC_DETECT_INVIS")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1002',string("NPC_NOTRACK")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(moralCharacter,'NpcCol1002',['MediumAmountFn','Goodness-Moral']),'*WorldStaticStateMt*').

%% pipequoted(";(osim-assert '(#$relationAllInstance #$skillLevel #$NpcCol1002 #$SingleDoerAction #$Effectiveness #$veryHighAmountOf) *WorldStaticStateMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )").
:-osimAssert(relationAllInstance(comment,'NpcCol1002',string("+mudToHitArmorClass0: 2")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(toughnessOfObject,'NpcCol1002','HighToVeryHighAmountFn'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1002',string("mudMaxHitPoints: 12d12+3400")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1002',string("#$PunchingSomething mudBareHandDamage: 9d9+60")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(netWorth,'NpcCol1002',['USDollarFn',10000]),'*WorldStaticStateMt*').

%% pipequoted(";(osim-assert '(#$relationAllInstance #$skillLevel #$NpcCol1002 #$IntentionalMentalEvent #$Effectiveness #$mediumToVeryHighAmountOf) *WorldStaticStateMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )").
:-osimAssert(possible(relationAllInstance(postureOfAnimal,'NpcCol1002','UprightPosture')),'*WorldStaticStateMt*').

:-osimAssert(genls('NpcCol1002','MaleAnimal'),'*WorldStaticStateMt*').

%% pipequoted("; mob").
%% pipequoted("; mob #1003").
'find-or-create-constant'(string("NpcCol1003")).

:-osimAssert(isa('NpcCol1003','BPVAgentType'),'*WorldVocabularyMt*').

:-osimAssert(genls('NpcCol1003','Agent-Generic'),'*WorldVocabularyMt*').

:-osimAssert(relationAllInstance(nicknames,'NpcCol1003',string("Doctor")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'NpcCol1003',string("Beverly")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'NpcCol1003',string("Crusher")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nameString,'NpcCol1003',string("Doctor Crusher")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(definiteDescriptions,'NpcCol1003',string("Lieutenant Beverly Crusher is here, looking for someone to heal")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(personalIndentifyingCharacteristic,'NpcCol1003',string("Doctor Crusher is the Enterprise's Chief Medical Officer")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(personalIndentifyingCharacteristic,'NpcCol1003',string("Wesley is her son")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(personalIndentifyingCharacteristic,'NpcCol1003',string("Her husband was killed years ago in an accident on another starship which was also commanded by Captain Picard")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1003',string("ACT_STAY_ZONE")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1003',string("MEMORY")),'*WorldStaticStateMt*').

:-osimAssert(frequencyOfActionType('RevengeAction','NpcCol1003',bodilyDoer,'Often'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1003',string("HELPER")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1003',string("ACT_FRIEND")),'*WorldStaticStateMt*').

:-osimAssert('typeBehaviorCapable-PerformedBy'('NpcCol1003','HelpingAnAgent'),'*WorldStaticStateMt*').

:-osimAssert(frequencyOfActionType('ProtectingSomething','NpcCol1003',bodilyDoer,'Often'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1003',string("NOCHARM")),'*WorldStaticStateMt*').

:-osimAssert(typeBehaviorIncapable('NpcCol1003','InfluencingAnAgent',recipientOfInfo),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1003',string("NOBASH")),'*WorldStaticStateMt*').

:-osimAssert(typeBehaviorIncapable('NpcCol1003','IncurringPhysicalDamage',damages),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1003',string("NOBLIND")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1003',string("NPC_DETECT_INVIS")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1003',string("NPC_NOTRACK")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(moralCharacter,'NpcCol1003',['MediumAmountFn','Goodness-Moral']),'*WorldStaticStateMt*').

%% pipequoted(";(osim-assert '(#$relationAllInstance #$skillLevel #$NpcCol1003 #$SingleDoerAction #$Effectiveness #$veryHighAmountOf) *WorldStaticStateMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )").
:-osimAssert(relationAllInstance(comment,'NpcCol1003',string("+mudToHitArmorClass0: 3")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(toughnessOfObject,'NpcCol1003','HighAmountFn'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1003',string("mudMaxHitPoints: 12d12+3200")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1003',string("#$PunchingSomething mudBareHandDamage: 9d9+42")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(netWorth,'NpcCol1003',['USDollarFn',75000]),'*WorldStaticStateMt*').

%% pipequoted(";(osim-assert '(#$relationAllInstance #$skillLevel #$NpcCol1003 #$IntentionalMentalEvent #$Effectiveness #$mediumAmountOf) *WorldStaticStateMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )").
:-osimAssert(possible(relationAllInstance(postureOfAnimal,'NpcCol1003','UprightPosture')),'*WorldStaticStateMt*').

:-osimAssert(genls('NpcCol1003','FemaleAnimal'),'*WorldStaticStateMt*').

%% pipequoted("; mob").
%% pipequoted("; mob #1004").
'find-or-create-constant'(string("NpcCol1004")).

:-osimAssert(isa('NpcCol1004','BPVAgentType'),'*WorldVocabularyMt*').

:-osimAssert(genls('NpcCol1004','Agent-Generic'),'*WorldVocabularyMt*').

:-osimAssert(relationAllInstance(nicknames,'NpcCol1004',string("Counselor")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'NpcCol1004',string("Deanna")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'NpcCol1004',string("Troi")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nameString,'NpcCol1004',string("Counselor Troi")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(definiteDescriptions,'NpcCol1004',string("Counselor Deanna Troi is here")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(personalIndentifyingCharacteristic,'NpcCol1004',string("Counselor Troi is the ship's main counselor")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(personalIndentifyingCharacteristic,'NpcCol1004',string("She's half betazoid, which means that she can read people's minds")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1004',string("ACT_STAY_ZONE")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1004',string("MEMORY")),'*WorldStaticStateMt*').

:-osimAssert(frequencyOfActionType('RevengeAction','NpcCol1004',bodilyDoer,'Often'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1004',string("HELPER")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1004',string("ACT_FRIEND")),'*WorldStaticStateMt*').

:-osimAssert('typeBehaviorCapable-PerformedBy'('NpcCol1004','HelpingAnAgent'),'*WorldStaticStateMt*').

:-osimAssert(frequencyOfActionType('ProtectingSomething','NpcCol1004',bodilyDoer,'Often'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1004',string("NOCHARM")),'*WorldStaticStateMt*').

:-osimAssert(typeBehaviorIncapable('NpcCol1004','InfluencingAnAgent',recipientOfInfo),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1004',string("NOBASH")),'*WorldStaticStateMt*').

:-osimAssert(typeBehaviorIncapable('NpcCol1004','IncurringPhysicalDamage',damages),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1004',string("NOBLIND")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1004',string("NPC_DETECT_INVIS")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1004',string("NPC_NOTRACK")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(moralCharacter,'NpcCol1004',['MediumAmountFn','Goodness-Moral']),'*WorldStaticStateMt*').

%% pipequoted(";(osim-assert '(#$relationAllInstance #$skillLevel #$NpcCol1004 #$SingleDoerAction #$Effectiveness #$veryHighAmountOf) *WorldStaticStateMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )").
:-osimAssert(relationAllInstance(comment,'NpcCol1004',string("+mudToHitArmorClass0: 3")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(toughnessOfObject,'NpcCol1004','HighAmountFn'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1004',string("mudMaxHitPoints: 12d12+3200")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1004',string("#$PunchingSomething mudBareHandDamage: 9d9+42")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(netWorth,'NpcCol1004',['USDollarFn',75000]),'*WorldStaticStateMt*').

%% pipequoted(";(osim-assert '(#$relationAllInstance #$skillLevel #$NpcCol1004 #$IntentionalMentalEvent #$Effectiveness #$mediumAmountOf) *WorldStaticStateMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )").
:-osimAssert(possible(relationAllInstance(postureOfAnimal,'NpcCol1004','UprightPosture')),'*WorldStaticStateMt*').

:-osimAssert(genls('NpcCol1004','FemaleAnimal'),'*WorldStaticStateMt*').

%% pipequoted("; mob").
%% pipequoted("; mob #1005").
'find-or-create-constant'(string("NpcCol1005")).

:-osimAssert(isa('NpcCol1005','BPVAgentType'),'*WorldVocabularyMt*').

:-osimAssert(genls('NpcCol1005','Agent-Generic'),'*WorldVocabularyMt*').

:-osimAssert(relationAllInstance(nicknames,'NpcCol1005',string("Commander")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'NpcCol1005',string("William")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'NpcCol1005',string("Riker")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nameString,'NpcCol1005',string("Commander Riker")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(definiteDescriptions,'NpcCol1005',string("Commander William Riker is here, staring at you")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(personalIndentifyingCharacteristic,'NpcCol1005',string("Commander Riker is the Enterprise's first officer")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(personalIndentifyingCharacteristic,'NpcCol1005',string("He's in charge of keeping the crew in line")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1005',string("ACT_STAY_ZONE")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1005',string("MEMORY")),'*WorldStaticStateMt*').

:-osimAssert(frequencyOfActionType('RevengeAction','NpcCol1005',bodilyDoer,'Often'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1005',string("HELPER")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1005',string("ACT_FRIEND")),'*WorldStaticStateMt*').

:-osimAssert('typeBehaviorCapable-PerformedBy'('NpcCol1005','HelpingAnAgent'),'*WorldStaticStateMt*').

:-osimAssert(frequencyOfActionType('ProtectingSomething','NpcCol1005',bodilyDoer,'Often'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1005',string("NOCHARM")),'*WorldStaticStateMt*').

:-osimAssert(typeBehaviorIncapable('NpcCol1005','InfluencingAnAgent',recipientOfInfo),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1005',string("NOBASH")),'*WorldStaticStateMt*').

:-osimAssert(typeBehaviorIncapable('NpcCol1005','IncurringPhysicalDamage',damages),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1005',string("NOBLIND")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1005',string("NPC_DETECT_INVIS")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1005',string("NPC_NOTRACK")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(moralCharacter,'NpcCol1005',['MediumAmountFn','Goodness-Moral']),'*WorldStaticStateMt*').

%% pipequoted(";(osim-assert '(#$relationAllInstance #$skillLevel #$NpcCol1005 #$SingleDoerAction #$Effectiveness #$veryHighAmountOf) *WorldStaticStateMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )").
:-osimAssert(relationAllInstance(comment,'NpcCol1005',string("+mudToHitArmorClass0: 2")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(toughnessOfObject,'NpcCol1005','HighToVeryHighAmountFn'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1005',string("mudMaxHitPoints: 12d12+3200")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1005',string("#$PunchingSomething mudBareHandDamage: 9d9+52")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(netWorth,'NpcCol1005',['USDollarFn',75000]),'*WorldStaticStateMt*').

%% pipequoted(";(osim-assert '(#$relationAllInstance #$skillLevel #$NpcCol1005 #$IntentionalMentalEvent #$Effectiveness #$mediumToVeryHighAmountOf) *WorldStaticStateMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )").
:-osimAssert(possible(relationAllInstance(postureOfAnimal,'NpcCol1005','UprightPosture')),'*WorldStaticStateMt*').

:-osimAssert(genls('NpcCol1005','MaleAnimal'),'*WorldStaticStateMt*').

%% pipequoted("; mob").
%% pipequoted("; mob #1006").
'find-or-create-constant'(string("NpcCol1006")).

:-osimAssert(isa('NpcCol1006','BPVAgentType'),'*WorldVocabularyMt*').

:-osimAssert(genls('NpcCol1006','Agent-Generic'),'*WorldVocabularyMt*').

:-osimAssert(relationAllInstance(nicknames,'NpcCol1006',string("Captain")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'NpcCol1006',string("Jean")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'NpcCol1006',string("Luc")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'NpcCol1006',string("Jean-Luc")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'NpcCol1006',string("Picard")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nameString,'NpcCol1006',string("Captain Picard")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(definiteDescriptions,'NpcCol1006',string("Captain Jean-Luc Picard is standing here, watching you")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(personalIndentifyingCharacteristic,'NpcCol1006',string("Captain Picard is a very important man")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(personalIndentifyingCharacteristic,'NpcCol1006',string("He's in charge of Starfleet's flagship, the Enterprise")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(personalIndentifyingCharacteristic,'NpcCol1006',string("He's very smart, and very wise")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(personalIndentifyingCharacteristic,'NpcCol1006',string("Don't mess with him!")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1006',string("ACT_NICE_THIEF")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1006',string("AWARE")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1006',string("NOBACKSTAB")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1006',string("ACT_STAY_ZONE")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1006',string("MEMORY")),'*WorldStaticStateMt*').

:-osimAssert(frequencyOfActionType('RevengeAction','NpcCol1006',bodilyDoer,'Often'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1006',string("HELPER")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1006',string("ACT_FRIEND")),'*WorldStaticStateMt*').

:-osimAssert('typeBehaviorCapable-PerformedBy'('NpcCol1006','HelpingAnAgent'),'*WorldStaticStateMt*').

:-osimAssert(frequencyOfActionType('ProtectingSomething','NpcCol1006',bodilyDoer,'Often'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1006',string("NOCHARM")),'*WorldStaticStateMt*').

:-osimAssert(typeBehaviorIncapable('NpcCol1006','InfluencingAnAgent',recipientOfInfo),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1006',string("NOSUMMON")),'*WorldStaticStateMt*').

:-osimAssert(typeBehaviorIncapable('NpcCol1006','CoercingAnAgentToMove',objectMoving),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1006',string("NOSLEEP")),'*WorldStaticStateMt*').

:-osimAssert(typeBehaviorIncapable('NpcCol1006','Sleeping',bodilyDoer),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1006',string("NOBASH")),'*WorldStaticStateMt*').

:-osimAssert(typeBehaviorIncapable('NpcCol1006','IncurringPhysicalDamage',damages),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1006',string("NOBLIND")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1006',string("NPC_DETECT_INVIS")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1006',string("NPC_SANCTUARY")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1006',string("NPC_NOTRACK")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(moralCharacter,'NpcCol1006',['MediumAmountFn','Goodness-Moral']),'*WorldStaticStateMt*').

%% pipequoted(";(osim-assert '(#$relationAllInstance #$skillLevel #$NpcCol1006 #$SingleDoerAction #$Effectiveness #$veryHighAmountOf) *WorldStaticStateMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )").
:-osimAssert(relationAllInstance(comment,'NpcCol1006',string("+mudToHitArmorClass0: 0")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(toughnessOfObject,'NpcCol1006','VeryHighAmountFn'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1006',string("mudMaxHitPoints: 20d20+5000")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1006',string("#$PunchingSomething mudBareHandDamage: 12d12+75")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(netWorth,'NpcCol1006',['USDollarFn',750000]),'*WorldStaticStateMt*').

%% pipequoted(";(osim-assert '(#$relationAllInstance #$skillLevel #$NpcCol1006 #$IntentionalMentalEvent #$Effectiveness #$veryHighAmountOf) *WorldStaticStateMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )").
:-osimAssert(possible(relationAllInstance(postureOfAnimal,'NpcCol1006','UprightPosture')),'*WorldStaticStateMt*').

:-osimAssert(genls('NpcCol1006','MaleAnimal'),'*WorldStaticStateMt*').

%% pipequoted("; mob").
%% pipequoted("; mob #1007").
'find-or-create-constant'(string("NpcCol1007")).

:-osimAssert(isa('NpcCol1007','BPVAgentType'),'*WorldVocabularyMt*').

:-osimAssert(genls('NpcCol1007','Agent-Generic'),'*WorldVocabularyMt*').

:-osimAssert(relationAllInstance(nicknames,'NpcCol1007',string("Guinan")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nameString,'NpcCol1007',string("Guinan")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(definiteDescriptions,'NpcCol1007',string("Guinan is here, tending the bar")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(personalIndentifyingCharacteristic,'NpcCol1007',string("Guinan is a strange being")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(personalIndentifyingCharacteristic,'NpcCol1007',string("She's lived for thousands of years and experienced many things, but now she's decided to work on the Enterprise as a bartender")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1007',string("ACT_SENTINEL")),'*WorldStaticStateMt*').

:-osimAssert(frequencyOfActionType('LeavingAPlace','NpcCol1007',bodilyDoer,'Never'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1007',string("ACT_STAY_ZONE")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1007',string("MEMORY")),'*WorldStaticStateMt*').

:-osimAssert(frequencyOfActionType('RevengeAction','NpcCol1007',bodilyDoer,'Often'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1007',string("HELPER")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1007',string("ACT_FRIEND")),'*WorldStaticStateMt*').

:-osimAssert('typeBehaviorCapable-PerformedBy'('NpcCol1007','HelpingAnAgent'),'*WorldStaticStateMt*').

:-osimAssert(frequencyOfActionType('ProtectingSomething','NpcCol1007',bodilyDoer,'Often'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1007',string("NOCHARM")),'*WorldStaticStateMt*').

:-osimAssert(typeBehaviorIncapable('NpcCol1007','InfluencingAnAgent',recipientOfInfo),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1007',string("NOBASH")),'*WorldStaticStateMt*').

:-osimAssert(typeBehaviorIncapable('NpcCol1007','IncurringPhysicalDamage',damages),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1007',string("NOBLIND")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1007',string("NPC_DETECT_INVIS")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1007',string("NPC_NOTRACK")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(moralCharacter,'NpcCol1007',['MediumAmountFn','Goodness-Moral']),'*WorldStaticStateMt*').

%% pipequoted(";(osim-assert '(#$relationAllInstance #$skillLevel #$NpcCol1007 #$SingleDoerAction #$Effectiveness #$veryHighAmountOf) *WorldStaticStateMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )").
:-osimAssert(relationAllInstance(comment,'NpcCol1007',string("+mudToHitArmorClass0: 4")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(toughnessOfObject,'NpcCol1007','MediumToVeryHighAmountFn'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1007',string("mudMaxHitPoints: 12d12+2600")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1007',string("#$PunchingSomething mudBareHandDamage: 9d9+36")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(netWorth,'NpcCol1007',['USDollarFn',50000]),'*WorldStaticStateMt*').

%% pipequoted(";(osim-assert '(#$relationAllInstance #$skillLevel #$NpcCol1007 #$IntentionalMentalEvent #$Effectiveness #$mediumAmountOf) *WorldStaticStateMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )").
:-osimAssert(possible(relationAllInstance(postureOfAnimal,'NpcCol1007','UprightPosture')),'*WorldStaticStateMt*').

:-osimAssert(genls('NpcCol1007','FemaleAnimal'),'*WorldStaticStateMt*').

%% pipequoted("; mob").
%% pipequoted("; mob #1008").
'find-or-create-constant'(string("NpcCol1008")).

:-osimAssert(isa('NpcCol1008','BPVAgentType'),'*WorldVocabularyMt*').

:-osimAssert(genls('NpcCol1008','Agent-Generic'),'*WorldVocabularyMt*').

:-osimAssert(relationAllInstance(nicknames,'NpcCol1008',string("Chief")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'NpcCol1008',string("O'Brien")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'NpcCol1008',string("Transporter")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nameString,'NpcCol1008',string("Chief O'Brien")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(definiteDescriptions,'NpcCol1008',string("Chief O'Brien is here, waiting to teleport you somwhere")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(personalIndentifyingCharacteristic,'NpcCol1008',string("Chief O'Brien is the transporter chief on the Enterprise")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(personalIndentifyingCharacteristic,'NpcCol1008',string("It's his job to make sure everyone arrives(and leaves) in one piece, instead of trillions of atoms")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1008',string("ACT_SENTINEL")),'*WorldStaticStateMt*').

:-osimAssert(frequencyOfActionType('LeavingAPlace','NpcCol1008',bodilyDoer,'Never'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1008',string("ACT_STAY_ZONE")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1008',string("MEMORY")),'*WorldStaticStateMt*').

:-osimAssert(frequencyOfActionType('RevengeAction','NpcCol1008',bodilyDoer,'Often'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1008',string("HELPER")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1008',string("ACT_FRIEND")),'*WorldStaticStateMt*').

:-osimAssert('typeBehaviorCapable-PerformedBy'('NpcCol1008','HelpingAnAgent'),'*WorldStaticStateMt*').

:-osimAssert(frequencyOfActionType('ProtectingSomething','NpcCol1008',bodilyDoer,'Often'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1008',string("NOCHARM")),'*WorldStaticStateMt*').

:-osimAssert(typeBehaviorIncapable('NpcCol1008','InfluencingAnAgent',recipientOfInfo),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1008',string("NOBASH")),'*WorldStaticStateMt*').

:-osimAssert(typeBehaviorIncapable('NpcCol1008','IncurringPhysicalDamage',damages),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1008',string("NOBLIND")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1008',string("NPC_DETECT_INVIS")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1008',string("NPC_NOTRACK")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(moralCharacter,'NpcCol1008',['MediumAmountFn','Goodness-Moral']),'*WorldStaticStateMt*').

%% pipequoted(";(osim-assert '(#$relationAllInstance #$skillLevel #$NpcCol1008 #$SingleDoerAction #$Effectiveness #$veryHighAmountOf) *WorldStaticStateMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )").
:-osimAssert(relationAllInstance(comment,'NpcCol1008',string("+mudToHitArmorClass0: 4")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(toughnessOfObject,'NpcCol1008','MediumToVeryHighAmountFn'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1008',string("mudMaxHitPoints: 12d12+2600")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1008',string("#$PunchingSomething mudBareHandDamage: 9d9+36")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(netWorth,'NpcCol1008',['USDollarFn',50000]),'*WorldStaticStateMt*').

%% pipequoted(";(osim-assert '(#$relationAllInstance #$skillLevel #$NpcCol1008 #$IntentionalMentalEvent #$Effectiveness #$mediumAmountOf) *WorldStaticStateMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )").
:-osimAssert(possible(relationAllInstance(postureOfAnimal,'NpcCol1008','UprightPosture')),'*WorldStaticStateMt*').

:-osimAssert(genls('NpcCol1008','MaleAnimal'),'*WorldStaticStateMt*').

%% pipequoted("; mob").
%% pipequoted("; mob #1009").
'find-or-create-constant'(string("NpcCol1009")).

:-osimAssert(isa('NpcCol1009','BPVAgentType'),'*WorldVocabularyMt*').

:-osimAssert(genls('NpcCol1009','Agent-Generic'),'*WorldVocabularyMt*').

:-osimAssert(relationAllInstance(nicknames,'NpcCol1009',string("Wesley")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'NpcCol1009',string("Crusher")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nameString,'NpcCol1009',string("Wesley")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(definiteDescriptions,'NpcCol1009',string("Wesley Crusher is here, eagerly trying to earn your praise")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(personalIndentifyingCharacteristic,'NpcCol1009',string("Wesley Crusher is not even an official officer, but he serves as an acting Ensign on the bridge")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(personalIndentifyingCharacteristic,'NpcCol1009',string("He got this position only because Captain Picard feels guilty about killing his father")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1009',string("ACT_STAY_ZONE")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1009',string("ACT_WIMPY")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1009',string("wimpy mobile will try to flee when it gets low on hit points. A mobile which is both aggressive and wimpy will not attack a player that is awake")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1009',string("MEMORY")),'*WorldStaticStateMt*').

:-osimAssert(frequencyOfActionType('RevengeAction','NpcCol1009',bodilyDoer,'Often'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1009',string("HELPER")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1009',string("ACT_FRIEND")),'*WorldStaticStateMt*').

:-osimAssert('typeBehaviorCapable-PerformedBy'('NpcCol1009','HelpingAnAgent'),'*WorldStaticStateMt*').

:-osimAssert(frequencyOfActionType('ProtectingSomething','NpcCol1009',bodilyDoer,'Often'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1009',string("NOCHARM")),'*WorldStaticStateMt*').

:-osimAssert(typeBehaviorIncapable('NpcCol1009','InfluencingAnAgent',recipientOfInfo),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1009',string("NOBASH")),'*WorldStaticStateMt*').

:-osimAssert(typeBehaviorIncapable('NpcCol1009','IncurringPhysicalDamage',damages),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1009',string("NOBLIND")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1009',string("NPC_DETECT_INVIS")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1009',string("NPC_NOTRACK")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(moralCharacter,'NpcCol1009',['MediumAmountFn','Goodness-Moral']),'*WorldStaticStateMt*').

%% pipequoted(";(osim-assert '(#$relationAllInstance #$skillLevel #$NpcCol1009 #$SingleDoerAction #$Effectiveness #$mediumToVeryHighAmountOf) *WorldStaticStateMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )").
:-osimAssert(relationAllInstance(comment,'NpcCol1009',string("+mudToHitArmorClass0: 6")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(toughnessOfObject,'NpcCol1009','LowAmountFn'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1009',string("mudMaxHitPoints: 12d12+1400")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1009',string("#$PunchingSomething mudBareHandDamage: 9d9+24")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(netWorth,'NpcCol1009',['USDollarFn',18000]),'*WorldStaticStateMt*').

%% pipequoted(";(osim-assert '(#$relationAllInstance #$skillLevel #$NpcCol1009 #$IntentionalMentalEvent #$Effectiveness #$veryLowToLowAmountOf) *WorldStaticStateMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )").
:-osimAssert(possible(relationAllInstance(postureOfAnimal,'NpcCol1009','UprightPosture')),'*WorldStaticStateMt*').

:-osimAssert(genls('NpcCol1009','MaleAnimal'),'*WorldStaticStateMt*').

%% pipequoted("; mob").
%% pipequoted("; mob #1010").
'find-or-create-constant'(string("NpcCol1010")).

:-osimAssert(isa('NpcCol1010','BPVAgentType'),'*WorldVocabularyMt*').

:-osimAssert(genls('NpcCol1010','Agent-Generic'),'*WorldVocabularyMt*').

:-osimAssert(relationAllInstance(nicknames,'NpcCol1010',string("Livingston")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'NpcCol1010',string("fish")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nameString,'NpcCol1010',string("Livingston")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(definiteDescriptions,'NpcCol1010',string("Livingston the fish is here, swimming about in his tank")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(personalIndentifyingCharacteristic,'NpcCol1010',string("Livingston is Captain Picard's pet fish")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(personalIndentifyingCharacteristic,'NpcCol1010',string("He's some sort of exotic breed, and he's expensive to feed and keep alive")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1010',string("ACT_SENTINEL")),'*WorldStaticStateMt*').

:-osimAssert(frequencyOfActionType('LeavingAPlace','NpcCol1010',bodilyDoer,'Never'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1010',string("ACT_STAY_ZONE")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1010',string("MEMORY")),'*WorldStaticStateMt*').

:-osimAssert(frequencyOfActionType('RevengeAction','NpcCol1010',bodilyDoer,'Often'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1010',string("HELPER")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1010',string("ACT_FRIEND")),'*WorldStaticStateMt*').

:-osimAssert('typeBehaviorCapable-PerformedBy'('NpcCol1010','HelpingAnAgent'),'*WorldStaticStateMt*').

:-osimAssert(frequencyOfActionType('ProtectingSomething','NpcCol1010',bodilyDoer,'Often'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1010',string("NOCHARM")),'*WorldStaticStateMt*').

:-osimAssert(typeBehaviorIncapable('NpcCol1010','InfluencingAnAgent',recipientOfInfo),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1010',string("NPC_DETECT_INVIS")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1010',string("NPC_NOTRACK")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(moralCharacter,'NpcCol1010',['MediumAmountFn','Goodness-Moral']),'*WorldStaticStateMt*').

%% pipequoted(";(osim-assert '(#$relationAllInstance #$skillLevel #$NpcCol1010 #$SingleDoerAction #$Effectiveness #$mediumAmountOf) *WorldStaticStateMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )").
:-osimAssert(relationAllInstance(comment,'NpcCol1010',string("+mudToHitArmorClass0: 6")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(toughnessOfObject,'NpcCol1010','NoAmountFn'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1010',string("mudMaxHitPoints: 12d12+800")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1010',string("#$PunchingSomething mudBareHandDamage: 9d9+14")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(netWorth,'NpcCol1010',['USDollarFn',5000]),'*WorldStaticStateMt*').

%% pipequoted(";(osim-assert '(#$relationAllInstance #$skillLevel #$NpcCol1010 #$IntentionalMentalEvent #$Effectiveness #$veryLowToLowAmountOf) *WorldStaticStateMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )").
:-osimAssert(possible(relationAllInstance(postureOfAnimal,'NpcCol1010','UprightPosture')),'*WorldStaticStateMt*').

:-osimAssert(genls('NpcCol1010','MaleAnimal'),'*WorldStaticStateMt*').

%% pipequoted("; mob").
%% pipequoted("; mob #1011").
'find-or-create-constant'(string("NpcCol1011")).

:-osimAssert(isa('NpcCol1011','BPVAgentType'),'*WorldVocabularyMt*').

:-osimAssert(genls('NpcCol1011','Agent-Generic'),'*WorldVocabularyMt*').

:-osimAssert(relationAllInstance(nicknames,'NpcCol1011',string("spot")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'NpcCol1011',string("the")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'NpcCol1011',string("cat")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nameString,'NpcCol1011',string("Spot")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(definiteDescriptions,'NpcCol1011',string("Spot, Data's pet cat, is sitting here looking at you")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(personalIndentifyingCharacteristic,'NpcCol1011',string("Spot is Data's orange coloured cat")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(personalIndentifyingCharacteristic,'NpcCol1011',string("Data is always trying to become more human, so he thinks that having a pet might help him achieve his goal")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1011',string("ACT_SENTINEL")),'*WorldStaticStateMt*').

:-osimAssert(frequencyOfActionType('LeavingAPlace','NpcCol1011',bodilyDoer,'Never'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1011',string("ACT_STAY_ZONE")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1011',string("MEMORY")),'*WorldStaticStateMt*').

:-osimAssert(frequencyOfActionType('RevengeAction','NpcCol1011',bodilyDoer,'Often'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1011',string("HELPER")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1011',string("ACT_FRIEND")),'*WorldStaticStateMt*').

:-osimAssert('typeBehaviorCapable-PerformedBy'('NpcCol1011','HelpingAnAgent'),'*WorldStaticStateMt*').

:-osimAssert(frequencyOfActionType('ProtectingSomething','NpcCol1011',bodilyDoer,'Often'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1011',string("NOCHARM")),'*WorldStaticStateMt*').

:-osimAssert(typeBehaviorIncapable('NpcCol1011','InfluencingAnAgent',recipientOfInfo),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1011',string("NPC_DETECT_INVIS")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1011',string("NPC_NOTRACK")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(moralCharacter,'NpcCol1011',['MediumAmountFn','Goodness-Moral']),'*WorldStaticStateMt*').

%% pipequoted(";(osim-assert '(#$relationAllInstance #$skillLevel #$NpcCol1011 #$SingleDoerAction #$Effectiveness #$mediumAmountOf) *WorldStaticStateMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )").
:-osimAssert(relationAllInstance(comment,'NpcCol1011',string("+mudToHitArmorClass0: 6")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(toughnessOfObject,'NpcCol1011','NoAmountFn'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1011',string("mudMaxHitPoints: 12d12+800")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1011',string("#$PunchingSomething mudBareHandDamage: 9d9+14")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(netWorth,'NpcCol1011',['USDollarFn',5000]),'*WorldStaticStateMt*').

%% pipequoted(";(osim-assert '(#$relationAllInstance #$skillLevel #$NpcCol1011 #$IntentionalMentalEvent #$Effectiveness #$veryLowToLowAmountOf) *WorldStaticStateMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )").
:-osimAssert(possible(relationAllInstance(postureOfAnimal,'NpcCol1011','UprightPosture')),'*WorldStaticStateMt*').

:-osimAssert(genls('NpcCol1011','MaleAnimal'),'*WorldStaticStateMt*').

%% pipequoted("; mob").
%% pipequoted("; mob #1012").
'find-or-create-constant'(string("NpcCol1012")).

:-osimAssert(isa('NpcCol1012','BPVAgentType'),'*WorldVocabularyMt*').

:-osimAssert(genls('NpcCol1012','Agent-Generic'),'*WorldVocabularyMt*').

:-osimAssert(relationAllInstance(nicknames,'NpcCol1012',string("ensign")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nameString,'NpcCol1012',string("the ensign")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(definiteDescriptions,'NpcCol1012',string("A nervous looking ensign is standing here, watching you")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(personalIndentifyingCharacteristic,'NpcCol1012',string("These ensigns make up the backbone of the Enterprise")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(personalIndentifyingCharacteristic,'NpcCol1012',string("They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1012',string("ACT_STAY_ZONE")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1012',string("MEMORY")),'*WorldStaticStateMt*').

:-osimAssert(frequencyOfActionType('RevengeAction','NpcCol1012',bodilyDoer,'Often'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1012',string("HELPER")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1012',string("ACT_FRIEND")),'*WorldStaticStateMt*').

:-osimAssert('typeBehaviorCapable-PerformedBy'('NpcCol1012','HelpingAnAgent'),'*WorldStaticStateMt*').

:-osimAssert(frequencyOfActionType('ProtectingSomething','NpcCol1012',bodilyDoer,'Often'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1012',string("NOCHARM")),'*WorldStaticStateMt*').

:-osimAssert(typeBehaviorIncapable('NpcCol1012','InfluencingAnAgent',recipientOfInfo),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1012',string("NOBASH")),'*WorldStaticStateMt*').

:-osimAssert(typeBehaviorIncapable('NpcCol1012','IncurringPhysicalDamage',damages),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1012',string("NOBLIND")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1012',string("NPC_DETECT_INVIS")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1012',string("NPC_NOTRACK")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(moralCharacter,'NpcCol1012',['MediumAmountFn','Goodness-Moral']),'*WorldStaticStateMt*').

%% pipequoted(";(osim-assert '(#$relationAllInstance #$skillLevel #$NpcCol1012 #$SingleDoerAction #$Effectiveness #$mediumAmountOf) *WorldStaticStateMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )").
:-osimAssert(relationAllInstance(comment,'NpcCol1012',string("+mudToHitArmorClass0: 6")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(toughnessOfObject,'NpcCol1012','NoAmountFn'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1012',string("mudMaxHitPoints: 8d8+1600")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1012',string("#$PunchingSomething mudBareHandDamage: 8d8+26")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(netWorth,'NpcCol1012',['USDollarFn',12000]),'*WorldStaticStateMt*').

%% pipequoted(";(osim-assert '(#$relationAllInstance #$skillLevel #$NpcCol1012 #$IntentionalMentalEvent #$Effectiveness #$veryLowToLowAmountOf) *WorldStaticStateMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )").
:-osimAssert(possible(relationAllInstance(postureOfAnimal,'NpcCol1012','UprightPosture')),'*WorldStaticStateMt*').

:-osimAssert(genls('NpcCol1012','MaleAnimal'),'*WorldStaticStateMt*').

%% pipequoted("; mob").
%% pipequoted("; mob #1013").
'find-or-create-constant'(string("NpcCol1013")).

:-osimAssert(isa('NpcCol1013','BPVAgentType'),'*WorldVocabularyMt*').

:-osimAssert(genls('NpcCol1013','Agent-Generic'),'*WorldVocabularyMt*').

:-osimAssert(relationAllInstance(nicknames,'NpcCol1013',string("alexander")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'NpcCol1013',string("rozhenko")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nameString,'NpcCol1013',string("alexander rozhenko")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(definiteDescriptions,'NpcCol1013',string("Alexander Rozhenko is here, practicing laughing hour")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(personalIndentifyingCharacteristic,'NpcCol1013',string("Alexander Rozhenko is Worf's son")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(personalIndentifyingCharacteristic,'NpcCol1013',string("His mother was half human and half Klingon, so Alexander is 3/4 Klingon")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(personalIndentifyingCharacteristic,'NpcCol1013',string("He's quite small, but since he's a Klingon he's very strong")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1013',string("ACT_STAY_ZONE")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1013',string("MEMORY")),'*WorldStaticStateMt*').

:-osimAssert(frequencyOfActionType('RevengeAction','NpcCol1013',bodilyDoer,'Often'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1013',string("HELPER")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1013',string("ACT_FRIEND")),'*WorldStaticStateMt*').

:-osimAssert('typeBehaviorCapable-PerformedBy'('NpcCol1013','HelpingAnAgent'),'*WorldStaticStateMt*').

:-osimAssert(frequencyOfActionType('ProtectingSomething','NpcCol1013',bodilyDoer,'Often'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1013',string("NOCHARM")),'*WorldStaticStateMt*').

:-osimAssert(typeBehaviorIncapable('NpcCol1013','InfluencingAnAgent',recipientOfInfo),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1013',string("NOBASH")),'*WorldStaticStateMt*').

:-osimAssert(typeBehaviorIncapable('NpcCol1013','IncurringPhysicalDamage',damages),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1013',string("NOBLIND")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1013',string("NPC_DETECT_INVIS")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1013',string("NPC_NOTRACK")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(moralCharacter,'NpcCol1013',['NoAmountFn','Goodness-Moral']),'*WorldStaticStateMt*').

%% pipequoted(";(osim-assert '(#$relationAllInstance #$skillLevel #$NpcCol1013 #$SingleDoerAction #$Effectiveness #$mediumAmountOf) *WorldStaticStateMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )").
:-osimAssert(relationAllInstance(comment,'NpcCol1013',string("+mudToHitArmorClass0: 6")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(toughnessOfObject,'NpcCol1013','NoAmountFn'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1013',string("mudMaxHitPoints: 8d8+1600")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'NpcCol1013',string("#$PunchingSomething mudBareHandDamage: 8d8+26")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(netWorth,'NpcCol1013',['USDollarFn',12000]),'*WorldStaticStateMt*').

%% pipequoted(";(osim-assert '(#$relationAllInstance #$skillLevel #$NpcCol1013 #$IntentionalMentalEvent #$Effectiveness #$veryLowToLowAmountOf) *WorldStaticStateMt* '(:DIRECTION :FORWARD :STRENGTH :MONOTONIC) )").
:-osimAssert(possible(relationAllInstance(postureOfAnimal,'NpcCol1013','UprightPosture')),'*WorldStaticStateMt*').

:-osimAssert(genls('NpcCol1013','MaleAnimal'),'*WorldStaticStateMt*').

%% pipequoted("; mob").
%% pipequoted("; mob").
%% pipequoted("load file '10.mob.kif'").
%% pipequoted("load file '10.mob.txt'").
%% pipequoted("load file '10.obj'").
'find-or-create-constant'(string("ArtifactCol1000")).

:-osimAssert(isa('ArtifactCol1000','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').

:-osimAssert(isa('ArtifactCol1000','BPVArtifactType'),'*WorldVocabularyMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1000',string("standard")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1000',string("issue")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1000',string("starfleet")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1000',string("phaser")),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1000','Handgun'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(termStrings,'ArtifactCol1000',string("a standard issue phaser")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1000',string("A standard issue Starfleet phaser has been left here")),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1000','Weapon'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1000','Weapon'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1000','Weapon'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1000',string("damageNumberDice 5")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1000',string("damageSizeDice 5")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1000',string("WeaponBlasting")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(stateOfDevice,'ArtifactCol1000','DeviceOn'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1000','LightingDevice'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1000','PortableObject'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1000','Device-SingleUser'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1000','SomethingToWear'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(massOfObject,'ArtifactCol1000',['Kilogram',3]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(cost,'ArtifactCol1000',['Dollar-UnitedStates',20000]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(cost,'ArtifactCol1000',['DollarsPerDay',500]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1000',string("These phasers are the standard weapon of Starfleet officers. It offers decent damage for its fairly small size")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1000',string("phaser")),'*WorldStaticStateMt*').

'find-or-create-constant'(string("ArtifactCol1001")).

:-osimAssert(isa('ArtifactCol1001','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').

:-osimAssert(isa('ArtifactCol1001','BPVArtifactType'),'*WorldVocabularyMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1001',string("phaser")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1001',string("rifle")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(termStrings,'ArtifactCol1001',string("a phaser rifle")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1001',string("A large phaser rifle is lying here")),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1001','Weapon'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1001','Weapon'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1001','Weapon'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1001',string("damageNumberDice 7")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1001',string("damageSizeDice 6")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1001',string("WeaponBlasting")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(stateOfDevice,'ArtifactCol1001','DeviceOn'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1001','LightingDevice'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1001','PortableObject'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1001','Device-SingleUser'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1001','SomethingToWear'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(massOfObject,'ArtifactCol1001',['Kilogram',8]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(cost,'ArtifactCol1001',['Dollar-UnitedStates',40000]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(cost,'ArtifactCol1001',['DollarsPerDay',1000]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1001',string("This phaser rifle looks pretty powerful. These weapons are used mainly on assault type missions, where power is important")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1001',string("phaser")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1001',string("rifle")),'*WorldStaticStateMt*').

'find-or-create-constant'(string("ArtifactCol1002")).

:-osimAssert(isa('ArtifactCol1002','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').

:-osimAssert(isa('ArtifactCol1002','BPVArtifactType'),'*WorldVocabularyMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1002',string("burgandy")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1002',string("starfleet")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1002',string("command")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1002',string("uniform")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(termStrings,'ArtifactCol1002',string("a burgandy Starfleet command uniform")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1002',string("A neatly folded burgandy Starfleet command uniform is lying here")),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1002','ProtectiveAttire'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1002',string("armorLevel: 10")),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1002','PortableObject'),'*WorldStaticStateMt*').

:-osimAssert('wornOn-TypeType'('ArtifactCol1002','Trunk-BodyCore'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1002','SomethingToWear'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(massOfObject,'ArtifactCol1002',['Kilogram',5]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(cost,'ArtifactCol1002',['Dollar-UnitedStates',12000]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(cost,'ArtifactCol1002',['DollarsPerDay',500]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1002',string("These uniforms are worn by command officers on Federation starships. It's kind of tight, but it looks pretty good")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1002',string("burgandy")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1002',string("command")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1002',string("uniform")),'*WorldStaticStateMt*').

'find-or-create-constant'(string("ArtifactCol1003")).

:-osimAssert(isa('ArtifactCol1003','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').

:-osimAssert(isa('ArtifactCol1003','BPVArtifactType'),'*WorldVocabularyMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1003',string("gold")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1003',string("starfleet")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1003',string("engineering")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1003',string("uniform")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(termStrings,'ArtifactCol1003',string("a gold Starfleet engineering uniform")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1003',string("A neatly folded gold Starfleet engineering uniform is lying here")),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1003','ProtectiveAttire'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1003',string("armorLevel: 10")),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1003','PortableObject'),'*WorldStaticStateMt*').

:-osimAssert('wornOn-TypeType'('ArtifactCol1003','Trunk-BodyCore'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1003','SomethingToWear'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(massOfObject,'ArtifactCol1003',['Kilogram',5]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(cost,'ArtifactCol1003',['Dollar-UnitedStates',12000]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(cost,'ArtifactCol1003',['DollarsPerDay',500]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1003',string("These uniforms are worn by engineering officers on Federation starships. It's kind of tight, but it looks pretty good")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1003',string("gold")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1003',string("engineering")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1003',string("uniform")),'*WorldStaticStateMt*').

'find-or-create-constant'(string("ArtifactCol1004")).

:-osimAssert(isa('ArtifactCol1004','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').

:-osimAssert(isa('ArtifactCol1004','BPVArtifactType'),'*WorldVocabularyMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1004',string("blue")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1004',string("starfleet")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1004',string("medical")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1004',string("uniform")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(termStrings,'ArtifactCol1004',string("a blue Starfleet medical uniform")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1004',string("A neatly folded blue Starfleet medical uniform is lying here")),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1004','ProtectiveAttire'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1004',string("armorLevel: 10")),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1004','PortableObject'),'*WorldStaticStateMt*').

:-osimAssert('wornOn-TypeType'('ArtifactCol1004','Trunk-BodyCore'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1004','SomethingToWear'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(massOfObject,'ArtifactCol1004',['Kilogram',5]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(cost,'ArtifactCol1004',['Dollar-UnitedStates',12000]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(cost,'ArtifactCol1004',['DollarsPerDay',500]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1004',string("These uniforms are worn by medical officers on Federation starships. It's kind of tight, but it looks pretty good")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1004',string("blue")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1004',string("medical")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1004',string("uniform")),'*WorldStaticStateMt*').

'find-or-create-constant'(string("ArtifactCol1005")).

:-osimAssert(isa('ArtifactCol1005','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').

:-osimAssert(isa('ArtifactCol1005','BPVArtifactType'),'*WorldVocabularyMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1005',string("starfleet")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1005',string("black")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1005',string("boots")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(termStrings,'ArtifactCol1005',string("a pair of Starfleet black boots")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1005',string("A pair of Starfleet black boots are sitting here")),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1005','ProtectiveAttire'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1005',string("armorLevel: 5")),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1005','PortableObject'),'*WorldStaticStateMt*').

:-osimAssert('wornOn-TypeType'('ArtifactCol1005','Foot-AnimalBodyPart'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1005','SomethingToWear'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(massOfObject,'ArtifactCol1005',['Kilogram',3]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(cost,'ArtifactCol1005',['Dollar-UnitedStates',8000]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(cost,'ArtifactCol1005',['DollarsPerDay',250]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1005',string("These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1005',string("starfleet")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1005',string("black")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1005',string("boots")),'*WorldStaticStateMt*').

'find-or-create-constant'(string("ArtifactCol1006")).

:-osimAssert(isa('ArtifactCol1006','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').

:-osimAssert(isa('ArtifactCol1006','BPVArtifactType'),'*WorldVocabularyMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1006',string("starfleet")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1006',string("comm")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1006',string("com")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1006',string("communication")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1006',string("badge")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(termStrings,'ArtifactCol1006',string("a Starfleet communication badge")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1006',string("A Starfleet communication badge is lying here")),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1006','ProtectiveAttire'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1006',string("armorLevel: 1")),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1006','PortableObject'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1006','Necklace'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(massOfObject,'ArtifactCol1006',['Kilogram',1]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(cost,'ArtifactCol1006',['Dollar-UnitedStates',20000]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(cost,'ArtifactCol1006',['DollarsPerDay',500]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1006',string("These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1006',string("starfleet")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1006',string("comm")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1006',string("com")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1006',string("communication")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1006',string("badge")),'*WorldStaticStateMt*').

'find-or-create-constant'(string("ArtifactCol1007")).

:-osimAssert(isa('ArtifactCol1007','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').

:-osimAssert(isa('ArtifactCol1007','BPVArtifactType'),'*WorldVocabularyMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1007',string("worf's")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1007',string("worf")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1007',string("sash")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(termStrings,'ArtifactCol1007',string("Worf's sash")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1007',string("Worf's silver chain sash has been left here")),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1007','ProtectiveAttire'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1007',string("armorLevel: 8")),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1007','PortableObject'),'*WorldStaticStateMt*').

:-osimAssert('wornOn-TypeType'('ArtifactCol1007','Torso'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1007','SomethingToWear'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(massOfObject,'ArtifactCol1007',['Kilogram',6]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(cost,'ArtifactCol1007',['Dollar-UnitedStates',15000]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(cost,'ArtifactCol1007',['DollarsPerDay',300]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1007',string("Worf's sash is some sort of Klingon clothing. Worf always wears it, which makes you wonder how you managed to get a hold of it..")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1007',string("worf's")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1007',string("worf")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1007',string("sash")),'*WorldStaticStateMt*').

'find-or-create-constant'(string("ArtifactCol1008")).

:-osimAssert(isa('ArtifactCol1008','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').

:-osimAssert(isa('ArtifactCol1008','BPVArtifactType'),'*WorldVocabularyMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1008',string("geordi")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1008',string("geordi's")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1008',string("visor")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(termStrings,'ArtifactCol1008',string("Geordi's VISOR")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1008',string("Geordi's VISOR is lying here")),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1008','ProtectiveAttire'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1008',string("armorLevel: 2")),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1008','PortableObject'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(massOfObject,'ArtifactCol1008',['Kilogram',6]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(cost,'ArtifactCol1008',['Dollar-UnitedStates',35000]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(cost,'ArtifactCol1008',['DollarsPerDay',750]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1008',string("Geordi's VISOR was made specially for him, because he's blind. This piece of equipment allows him to see things, but differently than normal eyes. I wonder how Geordi is managing, now that you've stolen his only way of seeing?")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1008',string("geordi")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1008',string("geordi's")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1008',string("visor")),'*WorldStaticStateMt*').

'find-or-create-constant'(string("ArtifactCol1009")).

:-osimAssert(isa('ArtifactCol1009','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').

:-osimAssert(isa('ArtifactCol1009','BPVArtifactType'),'*WorldVocabularyMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1009',string("medical")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1009',string("tricorder")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(termStrings,'ArtifactCol1009',string("a medical Tricorder")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1009',string("A medical Tricorder is lying here, ready to be used")),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1009','RodShapedObject'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1009','ControlDevice'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1009','HandTool'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1009','RodShapedObject'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1009','ControlDevice'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1009',string("mudLevelOf: 10")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1009',string("chargeCapacity: 5")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1009',string("chargeRemaining: 5")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1009',string("28")),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1009','PortableObject'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1009','HandTool'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1009','PortableObject'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(massOfObject,'ArtifactCol1009',['Kilogram',6]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(cost,'ArtifactCol1009',['Dollar-UnitedStates',15000]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(cost,'ArtifactCol1009',['DollarsPerDay',500]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1009',string("This medical Tricorder is used to heal small wounds and cuts. While it isn't made for major injuries, it can help you limp home. To use, hold it and then use it")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1009',string("medical")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1009',string("tricorder")),'*WorldStaticStateMt*').

'find-or-create-constant'(string("ArtifactCol1010")).

:-osimAssert(isa('ArtifactCol1010','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').

:-osimAssert(isa('ArtifactCol1010','BPVArtifactType'),'*WorldVocabularyMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1010',string("dilithium")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1010',string("crystal")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(termStrings,'ArtifactCol1010',string("a dilithium crystal")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1010',string("A shard of dilithium crystal is lying here")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1010',string("maybe a #$LightingDevice")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(duration,'ArtifactCol1010','-1'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(stateOfDevice,'ArtifactCol1010','DeviceOn'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1010','LightingDevice'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1010','PortableObject'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1010','HandTool'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1010','PortableObject'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(massOfObject,'ArtifactCol1010',['Kilogram',2]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(cost,'ArtifactCol1010',['Dollar-UnitedStates',8000]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(cost,'ArtifactCol1010',['DollarsPerDay',250]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1010',string("Dilithium crystals are used to power warp cores of starships. This particular crystal is glowing brightly, and gives off a blue-ish tinge")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1010',string("dilithium")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1010',string("crystal")),'*WorldStaticStateMt*').

'find-or-create-constant'(string("ArtifactCol1011")).

:-osimAssert(isa('ArtifactCol1011','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').

:-osimAssert(isa('ArtifactCol1011','BPVArtifactType'),'*WorldVocabularyMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1011',string("picard")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1011',string("picard's")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1011',string("flute")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(termStrings,'ArtifactCol1011',string("Picard's flute")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1011',string("Captain Picard's wooden flute is sitting here")),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1011','Artifact-Generic'),'*WorldVocabularyMt*').

:-osimAssert(relationAllInstance(stateOfDevice,'ArtifactCol1011','DeviceOn'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1011','InformationStore'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1011','PortableObject'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1011','HandTool'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1011','PortableObject'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(massOfObject,'ArtifactCol1011',['Kilogram',3]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(cost,'ArtifactCol1011',['Dollar-UnitedStates',8000]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(cost,'ArtifactCol1011',['DollarsPerDay',250]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1011',string("Captain Picard recieved this flute when he lost his memory and was stuck on some strange world. Now, he plays it to relieve stress")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1011',string("picard")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1011',string("picard's")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1011',string("flute")),'*WorldStaticStateMt*').

'find-or-create-constant'(string("ArtifactCol1012")).

:-osimAssert(isa('ArtifactCol1012','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').

:-osimAssert(isa('ArtifactCol1012','BPVArtifactType'),'*WorldVocabularyMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1012',string("riker")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1012',string("riker's")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1012',string("trombone")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(termStrings,'ArtifactCol1012',string("Riker's trombone")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1012',string("Commander Riker's trombone has been placed here")),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1012','Artifact-Generic'),'*WorldVocabularyMt*').

:-osimAssert(relationAllInstance(stateOfDevice,'ArtifactCol1012','DeviceOn'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1012','InformationStore'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1012','PortableObject'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1012','HandTool'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1012','PortableObject'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(massOfObject,'ArtifactCol1012',['Kilogram',5]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(cost,'ArtifactCol1012',['Dollar-UnitedStates',10000]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(cost,'ArtifactCol1012',['DollarsPerDay',250]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1012',string("Commander Riker considers himself to be a talented jazz musician. He practices on this trombone all the time")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1012',string("riker")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1012',string("riker's")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1012',string("trombone")),'*WorldStaticStateMt*').

'find-or-create-constant'(string("ArtifactCol1020")).

:-osimAssert(isa('ArtifactCol1020','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').

:-osimAssert(isa('ArtifactCol1020','BPVArtifactType'),'*WorldVocabularyMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1020',string("tea")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1020',string("cup")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(termStrings,'ArtifactCol1020',string("a small cup")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1020',string("A small cup of tea is sitting here")),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1020','Flask-LabGlassware'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1020','Bottle'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1020','FluidReservoir'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1020','Container'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(volumeOfObject,'ArtifactCol1020',['Liter',4]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(volumeContained,'ArtifactCol1020',['Liter',4]),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1020','Portal'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(portalState,'ArtifactCol1020','ClosedPortal'),'*WorldStaticStateMt*').

:-osimAssert(genls('Tea-Beverage','LiquidTangibleThing'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1020','PortableObject'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1020','HandTool'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1020','PortableObject'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(massOfObject,'ArtifactCol1020',['Kilogram',3]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(cost,'ArtifactCol1020',['Dollar-UnitedStates',15]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(cost,'ArtifactCol1020',['DollarsPerDay',10]),'*WorldStaticStateMt*').

'find-or-create-constant'(string("ArtifactCol1021")).

:-osimAssert(isa('ArtifactCol1021','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').

:-osimAssert(isa('ArtifactCol1021','BPVArtifactType'),'*WorldVocabularyMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1021',string("wine")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1021',string("bottle")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1021',string("synthehol")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(termStrings,'ArtifactCol1021',string("a synthehol")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1021',string("A bottle of synthehol is standing here")),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1021','Flask-LabGlassware'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1021','Bottle'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1021','FluidReservoir'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1021','Container'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(volumeOfObject,'ArtifactCol1021',['Liter',8]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(volumeContained,'ArtifactCol1021',['Liter',8]),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1021','Portal'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1021','LimitedAccess'),'*WorldStaticStateMt*').

:-osimAssert(genls('Wine','LiquidTangibleThing'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1021','PortableObject'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1021','HandTool'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1021','PortableObject'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(massOfObject,'ArtifactCol1021',['Kilogram',6]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(cost,'ArtifactCol1021',['Dollar-UnitedStates',30]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(cost,'ArtifactCol1021',['DollarsPerDay',10]),'*WorldStaticStateMt*').

'find-or-create-constant'(string("ArtifactCol1022")).

:-osimAssert(isa('ArtifactCol1022','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').

:-osimAssert(isa('ArtifactCol1022','BPVArtifactType'),'*WorldVocabularyMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1022',string("ale")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1022',string("ferengi")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1022',string("bottle")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(termStrings,'ArtifactCol1022',string("a Ferengi bottle")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1022',string("A bottle of Ferengi ale is sitting here")),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1022','Flask-LabGlassware'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1022','Bottle'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1022','FluidReservoir'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1022','Container'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(volumeOfObject,'ArtifactCol1022',['Liter',8]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(volumeContained,'ArtifactCol1022',['Liter',8]),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1022','Portal'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1022','LimitedAccess'),'*WorldStaticStateMt*').

:-osimAssert(genls('AleBeer','LiquidTangibleThing'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1022','PortableObject'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1022','HandTool'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1022','PortableObject'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(massOfObject,'ArtifactCol1022',['Kilogram',6]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(cost,'ArtifactCol1022',['Dollar-UnitedStates',30]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(cost,'ArtifactCol1022',['DollarsPerDay',10]),'*WorldStaticStateMt*').

'find-or-create-constant'(string("ArtifactCol1023")).

:-osimAssert(isa('ArtifactCol1023','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').

:-osimAssert(isa('ArtifactCol1023','BPVArtifactType'),'*WorldVocabularyMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1023',string("whisky")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1023',string("whiskey")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1023',string("romulan")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1023',string("bottle")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(termStrings,'ArtifactCol1023',string("a Romulan bottle")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1023',string("A bottle of Romulan whiskey is sitting here")),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1023','Flask-LabGlassware'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1023','Bottle'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1023','FluidReservoir'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1023','Container'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(volumeOfObject,'ArtifactCol1023',['Liter',8]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(volumeContained,'ArtifactCol1023',['Liter',8]),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1023','Portal'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1023','LimitedAccess'),'*WorldStaticStateMt*').

:-osimAssert(genls('Whisky','LiquidTangibleThing'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1023','PortableObject'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1023','HandTool'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1023','PortableObject'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(massOfObject,'ArtifactCol1023',['Kilogram',6]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(cost,'ArtifactCol1023',['Dollar-UnitedStates',45]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(cost,'ArtifactCol1023',['DollarsPerDay',10]),'*WorldStaticStateMt*').

'find-or-create-constant'(string("ArtifactCol1024")).

:-osimAssert(isa('ArtifactCol1024','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').

:-osimAssert(isa('ArtifactCol1024','BPVArtifactType'),'*WorldVocabularyMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1024',string("lemonade")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1024',string("prune")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1024',string("juice")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1024',string("glass")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(termStrings,'ArtifactCol1024',string("a small glass")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1024',string("A small glass of prune juice is sitting here")),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1024','Flask-LabGlassware'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1024','Bottle'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1024','FluidReservoir'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1024','Container'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(volumeOfObject,'ArtifactCol1024',['Liter',8]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(volumeContained,'ArtifactCol1024',['Liter',8]),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1024','Portal'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1024','LimitedAccess'),'*WorldStaticStateMt*').

:-osimAssert(genls('Lemonade','LiquidTangibleThing'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1024','PortableObject'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1024','HandTool'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1024','PortableObject'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(massOfObject,'ArtifactCol1024',['Kilogram',3]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(cost,'ArtifactCol1024',['Dollar-UnitedStates',10]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(cost,'ArtifactCol1024',['DollarsPerDay',10]),'*WorldStaticStateMt*').

'find-or-create-constant'(string("ArtifactCol1025")).

:-osimAssert(isa('ArtifactCol1025','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').

:-osimAssert(isa('ArtifactCol1025','BPVArtifactType'),'*WorldVocabularyMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1025',string("beer")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1025',string("vulcan")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(nicknames,'ArtifactCol1025',string("bottle")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(termStrings,'ArtifactCol1025',string("a Vulcan bottle")),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(comment,'ArtifactCol1025',string("A bottle of Vulcan beer is standing here")),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1025','Flask-LabGlassware'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1025','Bottle'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1025','FluidReservoir'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1025','Container'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(volumeOfObject,'ArtifactCol1025',['Liter',8]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(volumeContained,'ArtifactCol1025',['Liter',8]),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1025','Portal'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1025','LimitedAccess'),'*WorldStaticStateMt*').

:-osimAssert(genls('Beer','LiquidTangibleThing'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1025','PortableObject'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1025','HandTool'),'*WorldStaticStateMt*').

:-osimAssert(genls('ArtifactCol1025','PortableObject'),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(massOfObject,'ArtifactCol1025',['Kilogram',3]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(cost,'ArtifactCol1025',['Dollar-UnitedStates',10]),'*WorldStaticStateMt*').

:-osimAssert(relationAllInstance(cost,'ArtifactCol1025',['DollarsPerDay',10]),'*WorldStaticStateMt*').

%% pipequoted("load file '10.obj.kif'").
%% pipequoted("load file '10.shp'").
%% pipequoted("load file '10.wld'").
%% pipequoted("; Sourcing C:OpenSimsrcdaxmooworlds10.wld").
'find-or-create-constant'(string("Area1000")).

:-osimAssert(isa('Area1000','BPVLocation'),'*WorldStaticStateMt*').

:-osimAssert(nameString('Area1000',string("Main Engineering")),'*WorldStaticStateMt*').

:-osimAssert(nicknames('Area1000',string("Main")),'*WorldStaticStateMt*').

:-osimAssert(nicknames('Area1000',string("Engineering")),'*WorldStaticStateMt*').

:-osimAssert(definiteDescriptions('Area1000',string("You find yourself in the middle of main engineering")),'*WorldStaticStateMt*').

:-osimAssert(definiteDescriptions('Area1000',string("The room is longer than it is wide, and it has fairly low ceilings")),'*WorldStaticStateMt*').

:-osimAssert(definiteDescriptions('Area1000',string("Computer terminals cover all the walls, and a large table built into the floor sits in the middle of the room")),'*WorldStaticStateMt*').

:-osimAssert(definiteDescriptions('Area1000',string("At the far end of the room you see the warp core, a large pulsating vertical tube")),'*WorldStaticStateMt*').

:-osimAssert(isa('Area1000','Indoors-IsolatedFromOutside'),'*WorldStaticStateMt*').

:-osimAssert(isa('Area1000','SpaceInAHOC'),'*WorldStaticStateMt*').

'find-or-create-constant'(string("Area1002")).

:-osimAssert(isa('Area1002','BPVLocation'),'*WorldStaticStateMt*').

:-osimAssert(isa(['BoundsOfDirectionFn','Area1000','North-Directly'],'OpenPortal'),'*WorldStaticStateMt*').

:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1000','North-Directly'],'Area1000','Area1002'),'*WorldStaticStateMt*').

:-osimAssert(nameString(['BoundsOfDirectionFn','Area1000','North-Directly'],string("A corridor is North")),'*WorldStaticStateMt*').

:-osimAssert(isa(['BoundsOfDirectionFn','Area1000','East-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').

:-osimAssert(isa(['BoundsOfDirectionFn','Area1000','South-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').

:-osimAssert(isa(['BoundsOfDirectionFn','Area1000','West-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').

:-osimAssert(isa(['BoundsOfDirectionFn','Area1000','Up-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').

:-osimAssert(isa(['BoundsOfDirectionFn','Area1000','Down-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').

'find-or-create-constant'(string("Area1001")).

:-osimAssert(isa('Area1001','BPVLocation'),'*WorldStaticStateMt*').

:-osimAssert(nameString('Area1001',string("Geordi's Quarters")),'*WorldStaticStateMt*').

:-osimAssert(nicknames('Area1001',string("Geordi's")),'*WorldStaticStateMt*').

:-osimAssert(nicknames('Area1001',string("Quarters")),'*WorldStaticStateMt*').

:-osimAssert(definiteDescriptions('Area1001',string("You're in the middle of Geordi's quarters")),'*WorldStaticStateMt*').

:-osimAssert(definiteDescriptions('Area1001',string("The room is sparsely decorated, due to the fact that Geordi is blind")),'*WorldStaticStateMt*').

:-osimAssert(definiteDescriptions('Area1001',string("A small personal computer sits on a desk against the western wall, in between two windows that look out into space")),'*WorldStaticStateMt*').

:-osimAssert(definiteDescriptions('Area1001',string("A neatly made bed has been placed against the northern wall")),'*WorldStaticStateMt*').

:-osimAssert(isa('Area1001','Indoors-IsolatedFromOutside'),'*WorldStaticStateMt*').

:-osimAssert(isa('Area1001','SpaceInAHOC'),'*WorldStaticStateMt*').

'find-or-create-constant'(string("Area1002")).

:-osimAssert(isa('Area1002','BPVLocation'),'*WorldStaticStateMt*').

:-osimAssert(isa(['BoundsOfDirectionFn','Area1001','East-Directly'],'OpenPortal'),'*WorldStaticStateMt*').

:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1001','East-Directly'],'Area1001','Area1002'),'*WorldStaticStateMt*').

:-osimAssert(nameString(['BoundsOfDirectionFn','Area1001','East-Directly'],string("A corridor is East")),'*WorldStaticStateMt*').

:-osimAssert(isa(['BoundsOfDirectionFn','Area1001','North-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').

:-osimAssert(isa(['BoundsOfDirectionFn','Area1001','South-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').

:-osimAssert(isa(['BoundsOfDirectionFn','Area1001','West-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').

:-osimAssert(isa(['BoundsOfDirectionFn','Area1001','Up-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').

:-osimAssert(isa(['BoundsOfDirectionFn','Area1001','Down-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').

'find-or-create-constant'(string("Area1002")).

:-osimAssert(isa('Area1002','BPVLocation'),'*WorldStaticStateMt*').

:-osimAssert(nameString('Area1002',string("A Corridor")),'*WorldStaticStateMt*').

:-osimAssert(nicknames('Area1002',string("Corridor")),'*WorldStaticStateMt*').

:-osimAssert(definiteDescriptions('Area1002',string("You find yourself in the middle of a well lit corridor on the Enterprise")),'*WorldStaticStateMt*').

:-osimAssert(definiteDescriptions('Area1002',string("It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape")),'*WorldStaticStateMt*').

:-osimAssert(isa('Area1002','Indoors-IsolatedFromOutside'),'*WorldStaticStateMt*').

:-osimAssert(isa('Area1002','SpaceInAHOC'),'*WorldStaticStateMt*').

'find-or-create-constant'(string("Area1005")).

:-osimAssert(isa('Area1005','BPVLocation'),'*WorldStaticStateMt*').

:-osimAssert(isa(['BoundsOfDirectionFn','Area1002','North-Directly'],'OpenPortal'),'*WorldStaticStateMt*').

:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1002','North-Directly'],'Area1002','Area1005'),'*WorldStaticStateMt*').

:-osimAssert(nameString(['BoundsOfDirectionFn','Area1002','North-Directly'],string("The corridor continues North")),'*WorldStaticStateMt*').

'find-or-create-constant'(string("Area1003")).

:-osimAssert(isa('Area1003','BPVLocation'),'*WorldStaticStateMt*').

:-osimAssert(isa(['BoundsOfDirectionFn','Area1002','East-Directly'],'OpenPortal'),'*WorldStaticStateMt*').

:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1002','East-Directly'],'Area1002','Area1003'),'*WorldStaticStateMt*').

:-osimAssert(nameString(['BoundsOfDirectionFn','Area1002','East-Directly'],string("Data's Quarters are East")),'*WorldStaticStateMt*').

'find-or-create-constant'(string("Area1000")).

:-osimAssert(isa('Area1000','BPVLocation'),'*WorldStaticStateMt*').

:-osimAssert(isa(['BoundsOfDirectionFn','Area1002','South-Directly'],'OpenPortal'),'*WorldStaticStateMt*').

:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1002','South-Directly'],'Area1002','Area1000'),'*WorldStaticStateMt*').

:-osimAssert(nameString(['BoundsOfDirectionFn','Area1002','South-Directly'],string("Main Engineering is South")),'*WorldStaticStateMt*').

'find-or-create-constant'(string("Area1001")).

:-osimAssert(isa('Area1001','BPVLocation'),'*WorldStaticStateMt*').

:-osimAssert(isa(['BoundsOfDirectionFn','Area1002','West-Directly'],'OpenPortal'),'*WorldStaticStateMt*').

:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1002','West-Directly'],'Area1002','Area1001'),'*WorldStaticStateMt*').

:-osimAssert(nameString(['BoundsOfDirectionFn','Area1002','West-Directly'],string("Geordi's Quarters are West")),'*WorldStaticStateMt*').

:-osimAssert(isa(['BoundsOfDirectionFn','Area1002','Up-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').

:-osimAssert(isa(['BoundsOfDirectionFn','Area1002','Down-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').

'find-or-create-constant'(string("Area1003")).

:-osimAssert(isa('Area1003','BPVLocation'),'*WorldStaticStateMt*').

:-osimAssert(nameString('Area1003',string("Data's Quarters")),'*WorldStaticStateMt*').

:-osimAssert(nicknames('Area1003',string("Data's")),'*WorldStaticStateMt*').

:-osimAssert(nicknames('Area1003',string("Quarters")),'*WorldStaticStateMt*').

:-osimAssert(definiteDescriptions('Area1003',string("You're in the middle of Data's quarters")),'*WorldStaticStateMt*').

:-osimAssert(definiteDescriptions('Area1003',string("Some easils and paintings have been left scattered around the southern part of the room, while a huge computer screen showing a cross section of the Enterprise covers the entire northern wall")),'*WorldStaticStateMt*').

:-osimAssert(definiteDescriptions('Area1003',string("In front of the screen is a large desk, which is covered in computer controls")),'*WorldStaticStateMt*').

:-osimAssert(definiteDescriptions('Area1003',string("You can't see a bed in this room, but you figure it's because Data doesn't sleep")),'*WorldStaticStateMt*').

:-osimAssert(isa('Area1003','Indoors-IsolatedFromOutside'),'*WorldStaticStateMt*').

:-osimAssert(isa('Area1003','SpaceInAHOC'),'*WorldStaticStateMt*').

'find-or-create-constant'(string("Area1002")).

:-osimAssert(isa('Area1002','BPVLocation'),'*WorldStaticStateMt*').

:-osimAssert(isa(['BoundsOfDirectionFn','Area1003','West-Directly'],'OpenPortal'),'*WorldStaticStateMt*').

:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1003','West-Directly'],'Area1003','Area1002'),'*WorldStaticStateMt*').

:-osimAssert(nameString(['BoundsOfDirectionFn','Area1003','West-Directly'],string("A corridor is West")),'*WorldStaticStateMt*').

:-osimAssert(isa(['BoundsOfDirectionFn','Area1003','North-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').

:-osimAssert(isa(['BoundsOfDirectionFn','Area1003','East-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').

:-osimAssert(isa(['BoundsOfDirectionFn','Area1003','South-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').

:-osimAssert(isa(['BoundsOfDirectionFn','Area1003','Up-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').

:-osimAssert(isa(['BoundsOfDirectionFn','Area1003','Down-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').

'find-or-create-constant'(string("Area1004")).

:-osimAssert(isa('Area1004','BPVLocation'),'*WorldStaticStateMt*').

:-osimAssert(nameString('Area1004',string("The Brig")),'*WorldStaticStateMt*').

:-osimAssert(nicknames('Area1004',string("Brig")),'*WorldStaticStateMt*').

:-osimAssert(definiteDescriptions('Area1004',string("You're in the dimly lit Brig")),'*WorldStaticStateMt*').

:-osimAssert(definiteDescriptions('Area1004',string("This is where all the criminals and prisoners are kept while on board the Enterprise")),'*WorldStaticStateMt*').

:-osimAssert(definiteDescriptions('Area1004',string("Three fairly large cells can been seen in the southern part of the room, and they're all empty")),'*WorldStaticStateMt*').

:-osimAssert(definiteDescriptions('Area1004',string("A computer control panel is situated in the northwestern corner of the room, which is where the force fields for the cells are controlled")),'*WorldStaticStateMt*').

:-osimAssert(isa('Area1004','Indoors-IsolatedFromOutside'),'*WorldStaticStateMt*').

:-osimAssert(isa('Area1004','SpaceInAHOC'),'*WorldStaticStateMt*').

'find-or-create-constant'(string("Area1005")).

:-osimAssert(isa('Area1005','BPVLocation'),'*WorldStaticStateMt*').

:-osimAssert(isa(['BoundsOfDirectionFn','Area1004','East-Directly'],'OpenPortal'),'*WorldStaticStateMt*').

:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1004','East-Directly'],'Area1004','Area1005'),'*WorldStaticStateMt*').

:-osimAssert(nameString(['BoundsOfDirectionFn','Area1004','East-Directly'],string("A corridor is East")),'*WorldStaticStateMt*').

:-osimAssert(isa(['BoundsOfDirectionFn','Area1004','North-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').

:-osimAssert(isa(['BoundsOfDirectionFn','Area1004','South-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').

:-osimAssert(isa(['BoundsOfDirectionFn','Area1004','West-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').

:-osimAssert(isa(['BoundsOfDirectionFn','Area1004','Up-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').

:-osimAssert(isa(['BoundsOfDirectionFn','Area1004','Down-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').

'find-or-create-constant'(string("Area1005")).

:-osimAssert(isa('Area1005','BPVLocation'),'*WorldStaticStateMt*').

:-osimAssert(nameString('Area1005',string("A Corridor")),'*WorldStaticStateMt*').

:-osimAssert(nicknames('Area1005',string("Corridor")),'*WorldStaticStateMt*').

:-osimAssert(definiteDescriptions('Area1005',string("You find yourself in the middle of a well lit corridor on the Enterprise")),'*WorldStaticStateMt*').

:-osimAssert(definiteDescriptions('Area1005',string("It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape")),'*WorldStaticStateMt*').

:-osimAssert(definiteDescriptions('Area1005',string("You notice a tiny computer panel embedded into the wall")),'*WorldStaticStateMt*').

:-osimAssert(isa('Area1005','Indoors-IsolatedFromOutside'),'*WorldStaticStateMt*').

:-osimAssert(isa('Area1005','SpaceInAHOC'),'*WorldStaticStateMt*').

'find-or-create-constant'(string("Area1008")).

:-osimAssert(isa('Area1008','BPVLocation'),'*WorldStaticStateMt*').

:-osimAssert(isa(['BoundsOfDirectionFn','Area1005','North-Directly'],'OpenPortal'),'*WorldStaticStateMt*').

:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1005','North-Directly'],'Area1005','Area1008'),'*WorldStaticStateMt*').

:-osimAssert(nameString(['BoundsOfDirectionFn','Area1005','North-Directly'],string("The corridor continues North")),'*WorldStaticStateMt*').

'find-or-create-constant'(string("Area1006")).

:-osimAssert(isa('Area1006','BPVLocation'),'*WorldStaticStateMt*').

:-osimAssert(isa(['BoundsOfDirectionFn','Area1005','East-Directly'],'OpenPortal'),'*WorldStaticStateMt*').

:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1005','East-Directly'],'Area1005','Area1006'),'*WorldStaticStateMt*').

:-osimAssert(nameString(['BoundsOfDirectionFn','Area1005','East-Directly'],string("The Transporter Room is East")),'*WorldStaticStateMt*').

'find-or-create-constant'(string("Area1002")).

:-osimAssert(isa('Area1002','BPVLocation'),'*WorldStaticStateMt*').

:-osimAssert(isa(['BoundsOfDirectionFn','Area1005','South-Directly'],'OpenPortal'),'*WorldStaticStateMt*').

:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1005','South-Directly'],'Area1005','Area1002'),'*WorldStaticStateMt*').

:-osimAssert(nameString(['BoundsOfDirectionFn','Area1005','South-Directly'],string("The corridor continues South")),'*WorldStaticStateMt*').

'find-or-create-constant'(string("Area1004")).

:-osimAssert(isa('Area1004','BPVLocation'),'*WorldStaticStateMt*').

:-osimAssert(isa(['BoundsOfDirectionFn','Area1005','West-Directly'],'OpenPortal'),'*WorldStaticStateMt*').

:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1005','West-Directly'],'Area1005','Area1004'),'*WorldStaticStateMt*').

:-osimAssert(nameString(['BoundsOfDirectionFn','Area1005','West-Directly'],string("The Brig is West")),'*WorldStaticStateMt*').

'find-or-create-constant'(string("Area1005-Object666")).

:-osimAssert(isa('Area1005-Object666','PartiallyTangible'),'*WorldStaticStateMt*').

:-osimAssert(definiteDescriptions('Area1005-Object666',string("The panel says: <br> <br>*************************************************** <br>* * <br>* NCC-1701-D - 'ENTERPRISE' * <br>* * <br>* ***** * <br>* ********************** * <br>* *********************** _________ * <br>* ***** ***(___ ____( * <br>* *****  * * <br>* ********** * <br>* * <br>* You are currently on deck 1 * <br>* * <br>***************************************************")),'*WorldStaticStateMt*').
:-osimAssert('in-ContCompletely'('Area1005-Object666','Area1005'),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1005-Object666',string("control")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1005-Object666',string("panel")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1005-Object666',string("computer")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1005-Object666',string("screen")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1005-Object666',string("sign")),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1005','Up-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1005','Down-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1006")).
:-osimAssert(isa('Area1006','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(nameString('Area1006',string("Transporter Room")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1006',string("Transporter")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1006',string("Room")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1006',string("You're in the Enterprise transporter room")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1006',string("A computer terminal is sitting near the southern wall, where the transporter chief can control the transporters")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1006',string("Eight round transport pads have been arranged in a circle, on a raised platform against the northern wall")),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1006','Indoors-IsolatedFromOutside'),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1006','SpaceInAHOC'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1042")).
:-osimAssert(isa('Area1042','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1006','North-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1006','North-Directly'],'Area1006','Area1042'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1006','North-Directly'],string("A transporter beam is North")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1005")).
:-osimAssert(isa('Area1005','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1006','West-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1006','West-Directly'],'Area1006','Area1005'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1006','West-Directly'],string("A corridor is West")),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1006','East-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1006','South-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1006','Up-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1006','Down-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1007")).
:-osimAssert(isa('Area1007','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(nameString('Area1007',string("School")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1007',string("School")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1007',string("You step through the doors and find yourself in a large school room")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1007',string("Various tables and chairs are set up all around the room, and many paintings and drawings have been attached to the walls")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1007',string("Several computer consoles with a children's interface on them can be seen on the tables")),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1007','Indoors-IsolatedFromOutside'),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1007','SpaceInAHOC'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1008")).
:-osimAssert(isa('Area1008','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1007','East-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1007','East-Directly'],'Area1007','Area1008'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1007','East-Directly'],string("A corridor is East")),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1007','North-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1007','South-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1007','West-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1007','Up-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1007','Down-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1008")).
:-osimAssert(isa('Area1008','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(nameString('Area1008',string("A Corridor")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1008',string("Corridor")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1008',string("You find yourself in the middle of a well lit corridor on the Enterprise")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1008',string("It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1008',string("You see the holodeck's control panel beside the holodeck door, and it has some information on it")),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1008','Indoors-IsolatedFromOutside'),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1008','SpaceInAHOC'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1010")).
:-osimAssert(isa('Area1010','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1008','North-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1008','North-Directly'],'Area1008','Area1010'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1008','North-Directly'],string("The turbolift is North")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1009")).
:-osimAssert(isa('Area1009','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1008','East-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1008','East-Directly'],'Area1008','Area1009'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1008','East-Directly'],string("Holodeck 2 is East")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1005")).
:-osimAssert(isa('Area1005','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1008','South-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1008','South-Directly'],'Area1008','Area1005'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1008','South-Directly'],string("The corridor continues South")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1007")).
:-osimAssert(isa('Area1007','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1008','West-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1008','West-Directly'],'Area1008','Area1007'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1008','West-Directly'],string("The School is West")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1008-Object667")).
:-osimAssert(isa('Area1008-Object667','PartiallyTangible'),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1008-Object667',string("It looks like this: <br> <br>*************************************************** <br>* * <br>* NCC-1701-D - 'ENTERPRISE' * <br>* HOLODECK 2 * <br>* * <br>* STATUS : Inactive * <br>* CURRENT PROGRAM : N/A * <br>* SAFETIES : N/A * <br>* * <br>* NOTE: Starfleet is not responsible for * <br>* any injuries incurred while on this * <br>* holodeck! * <br>* * <br>* WARNING: While the safeties are disabled, you * <br>* CAN be injured, or even killed. * <br>* * <br>***************************************************")),'*WorldStaticStateMt*').
:-osimAssert('in-ContCompletely'('Area1008-Object667','Area1008'),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1008-Object667',string("holodeck")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1008-Object667',string("computer")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1008-Object667',string("control")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1008-Object667',string("panel")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1008-Object667',string("sign")),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1008','Up-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1008','Down-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1009")).
:-osimAssert(isa('Area1009','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(nameString('Area1009',string("Holodeck 2")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1009',string("Holodeck")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1009',string("You're now on Holodeck 2")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1009',string("The room is just a large cube, with jet black walls and a yellow grid painted on the floors, the walls, and the ceiling")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1009',string("This is where different programs can be loaded and experienced, which seem totally real")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1009',string("Right now, this holodeck is not functioning")),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1009','Indoors-IsolatedFromOutside'),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1009','SpaceInAHOC'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1008")).
:-osimAssert(isa('Area1008','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1009','West-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1009','West-Directly'],'Area1009','Area1008'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1009','West-Directly'],string("A corridor is West")),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1009','North-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1009','East-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1009','South-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1009','Up-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1009','Down-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1010")).
:-osimAssert(isa('Area1010','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(nameString('Area1010',string("Turbolift")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1010',string("Turbolift")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1010',string("You're in the turbolift")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1010',string("The turbolift walls have been rounded off, making it in the shape of a tube")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1010',string("Several vertical rows of lights make this place very well lit")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1010',string("From here, you can access the other decks on the Enterprise")),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1010','Indoors-IsolatedFromOutside'),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1010','SpaceInAHOC'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1008")).
:-osimAssert(isa('Area1008','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1010','South-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1010','South-Directly'],'Area1010','Area1008'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1010','South-Directly'],string("A corridor leads South")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1011")).
:-osimAssert(isa('Area1011','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1010','Up-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1010','Up-Directly'],'Area1010','Area1011'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1010','Up-Directly'],string("The turbolift goes Up")),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1010','North-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1010','East-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1010','West-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1010','Down-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1011")).
:-osimAssert(isa('Area1011','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(nameString('Area1011',string("Turbolift")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1011',string("Turbolift")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1011',string("You're in the turbolift")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1011',string("The turbolift walls have been rounded off, making it in the shape of a tube")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1011',string("Several vertical rows of lights make this place very well lit")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1011',string("From here, you can accessthe other decks on the Enterprise")),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1011','Indoors-IsolatedFromOutside'),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1011','SpaceInAHOC'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1013")).
:-osimAssert(isa('Area1013','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1011','North-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1011','North-Directly'],'Area1011','Area1013'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1011','North-Directly'],string("A corridor leads North")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1032")).
:-osimAssert(isa('Area1032','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1011','Up-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1011','Up-Directly'],'Area1011','Area1032'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1011','Up-Directly'],string("The turbolift goes Up")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1010")).
:-osimAssert(isa('Area1010','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1011','Down-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1011','Down-Directly'],'Area1011','Area1010'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1011','Down-Directly'],string("The turbolift goes Down")),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1011','East-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1011','South-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1011','West-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1012")).
:-osimAssert(isa('Area1012','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(nameString('Area1012',string("Cargo Bay 1")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1012',string("Cargo")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1012',string("Bay")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1012',string("You're in the main cargo bay of the Enterprise")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1012',string("It's quite a large room, with a very high ceiling and a lot of floor space")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1012',string("You can see several hundred plastic crates and barrels with the Starfleet insignia on them stacked right up to the ceiling")),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1012','Indoors-IsolatedFromOutside'),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1012','SpaceInAHOC'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1013")).
:-osimAssert(isa('Area1013','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1012','East-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1012','East-Directly'],'Area1012','Area1013'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1012','East-Directly'],string("A corridor is East")),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1012','North-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1012','South-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1012','West-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1012','Up-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1012','Down-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1013")).
:-osimAssert(isa('Area1013','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(nameString('Area1013',string("A Corridor")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1013',string("Corridor")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1013',string("You find yourself in the middle of a well lit corridor on the Enterprise")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1013',string("It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1013',string("You notice a tiny computer panel embedded into the wall")),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1013','Indoors-IsolatedFromOutside'),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1013','SpaceInAHOC'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1016")).
:-osimAssert(isa('Area1016','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1013','North-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1013','North-Directly'],'Area1013','Area1016'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1013','North-Directly'],string("The corridor continues North")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1014")).
:-osimAssert(isa('Area1014','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1013','East-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1013','East-Directly'],'Area1013','Area1014'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1013','East-Directly'],string("Riker's Quarters are East")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1011")).
:-osimAssert(isa('Area1011','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1013','South-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1013','South-Directly'],'Area1013','Area1011'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1013','South-Directly'],string("The Turbolift is South")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1012")).
:-osimAssert(isa('Area1012','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1013','West-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1013','West-Directly'],'Area1013','Area1012'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1013','West-Directly'],string("The Cargo Bay is West")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1013-Object668")).
:-osimAssert(isa('Area1013-Object668','PartiallyTangible'),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1013-Object668',string("The panel says: <br> <br>*************************************************** <br>* * <br>* NCC-1701-D - 'ENTERPRISE' * <br>* * <br>* ***** * <br>* ********************** * <br>* *********************** _________ * <br>* ***** ***(___ ____( * <br>* *****  * * <br>* ********** * <br>* * <br>* You are currently on deck 2 * <br>* * <br>***************************************************")),'*WorldStaticStateMt*').
:-osimAssert('in-ContCompletely'('Area1013-Object668','Area1013'),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1013-Object668',string("control")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1013-Object668',string("panel")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1013-Object668',string("computer")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1013-Object668',string("screen")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1013-Object668',string("sign")),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1013','Up-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1013','Down-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1014")).
:-osimAssert(isa('Area1014','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(nameString('Area1014',string("Riker's Quarters")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1014',string("Riker's")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1014',string("Quarters")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1014',string("You've arrived in Riker's quarters")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1014',string("The room is very neat and tidy, with a couch and several chairs aranged around a coffee table by the eastern wall")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1014',string("A small partition at the northern part of the room seperates his sleeping area with the rest of the room")),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1014','Indoors-IsolatedFromOutside'),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1014','SpaceInAHOC'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1013")).
:-osimAssert(isa('Area1013','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1014','West-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1014','West-Directly'],'Area1014','Area1013'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1014','West-Directly'],string("A corridor is West")),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1014','North-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1014','East-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1014','South-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1014','Up-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1014','Down-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1015")).
:-osimAssert(isa('Area1015','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(nameString('Area1015',string("Sick Bay")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1015',string("Sick")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1015',string("Bay")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1015',string("You're in the middle of the Enterprise's Sick Bay")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1015',string("About a dozen beds are arranged along the walls of the room, while several carts covered with medical supplies are scattered around the room")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1015',string("A large glass window in the northern part of the room separates the doctor's office with the rest of the room")),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1015','Indoors-IsolatedFromOutside'),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1015','SpaceInAHOC'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1016")).
:-osimAssert(isa('Area1016','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1015','East-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1015','East-Directly'],'Area1015','Area1016'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1015','East-Directly'],string("A corridor is East")),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1015','North-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1015','South-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1015','West-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1015','Up-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1015','Down-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1016")).
:-osimAssert(isa('Area1016','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(nameString('Area1016',string("A Corridor")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1016',string("Corridor")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1016',string("You find yourself in the middle of a well lit corridor on the Enterprise")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1016',string("It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1016',string("You see the holodeck's control panel beside the holodeck door, and it has some information on it")),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1016','Indoors-IsolatedFromOutside'),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1016','SpaceInAHOC'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1019")).
:-osimAssert(isa('Area1019','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1016','North-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1016','North-Directly'],'Area1016','Area1019'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1016','North-Directly'],string("The corridor continues North")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1017")).
:-osimAssert(isa('Area1017','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1016','East-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1016','East-Directly'],'Area1016','Area1017'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1016','East-Directly'],string("Holodeck 4 is East")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1013")).
:-osimAssert(isa('Area1013','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1016','South-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1016','South-Directly'],'Area1016','Area1013'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1016','South-Directly'],string("The corridor continues South")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1015")).
:-osimAssert(isa('Area1015','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1016','West-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1016','West-Directly'],'Area1016','Area1015'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1016','West-Directly'],string("Sick Bay is West")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1016-Object669")).
:-osimAssert(isa('Area1016-Object669','PartiallyTangible'),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1016-Object669',string("It looks like this: <br> <br>*************************************************** <br>* * <br>* NCC-1701-D - 'ENTERPRISE' * <br>* HOLODECK 4 * <br>* * <br>* STATUS : Active * <br>* CURRENT PROGRAM : Sherlock Holmes(19th * <br>* century London) * <br>* SAFETIES : Disabled * <br>* * <br>* NOTE: Starfleet is not responsible for * <br>* any injuries incurred while on this * <br>* holodeck! * <br>* * <br>* WARNING: While the safeties are disabled, you * <br>* CAN be injured, or even killed. * <br>* * <br>* -ENTER WHEN READY- * <br>* * <br>***************************************************")),'*WorldStaticStateMt*').
:-osimAssert('in-ContCompletely'('Area1016-Object669','Area1016'),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1016-Object669',string("holodeck")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1016-Object669',string("computer")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1016-Object669',string("control")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1016-Object669',string("panel")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1016-Object669',string("sign")),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1016','Up-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1016','Down-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1017")).
:-osimAssert(isa('Area1017','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(nameString('Area1017',string("Holodeck 4 Entrance - A Narrow Alley")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1017',string("Holodeck")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1017',string("Entrance")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1017',string("Narrow")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1017',string("Alley")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1017',string("You emerge into a dark narrow alley")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1017',string("Tall dark brick buildings block your way north and south")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1017',string("You can see that the windows on the buildings are fairly high, and some have been boarded up")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1017',string("The smell from the rotting food and garbage mixing with the foul water on the ground is unbearable")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1017',string("You can hear the sounds of a bustling marketpace to the east")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1017',string("The archway leading out of the holodeck is west")),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1017','Indoors-IsolatedFromOutside'),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1017','SpaceInAHOC'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1016")).
:-osimAssert(isa('Area1016','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1017','West-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1017','West-Directly'],'Area1017','Area1016'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1017','West-Directly'],string("A corridor is West")),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1017','North-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1017','East-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1017','South-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1017','Up-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1017','Down-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1018")).
:-osimAssert(isa('Area1018','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(nameString('Area1018',string("Crusher's Quarters")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1018',string("Crusher's")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1018',string("Quarters")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1018',string("You're in Doctor Crusher's quarters")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1018',string("Several different paintings are attached to the walls, and you also notice a few sculptures")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1018',string("A neatly made bed is located by the northern wall, in between two large windows looking out into space")),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1018','Indoors-IsolatedFromOutside'),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1018','SpaceInAHOC'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1019")).
:-osimAssert(isa('Area1019','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1018','East-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1018','East-Directly'],'Area1018','Area1019'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1018','East-Directly'],string("A corridor is East")),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1018','North-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1018','South-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1018','West-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1018','Up-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1018','Down-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1019")).
:-osimAssert(isa('Area1019','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(nameString('Area1019',string("A Corridor")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1019',string("Corridor")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1019',string("You find yourself in the middle of a well lit corridor on the Enterprise")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1019',string("It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape")),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1019','Indoors-IsolatedFromOutside'),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1019','SpaceInAHOC'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1021")).
:-osimAssert(isa('Area1021','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1019','North-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1019','North-Directly'],'Area1019','Area1021'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1019','North-Directly'],string("Ten Forward is North")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1020")).
:-osimAssert(isa('Area1020','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1019','East-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1019','East-Directly'],'Area1019','Area1020'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1019','East-Directly'],string("Security is East")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1016")).
:-osimAssert(isa('Area1016','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1019','South-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1019','South-Directly'],'Area1019','Area1016'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1019','South-Directly'],string("The corridor continues South")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1018")).
:-osimAssert(isa('Area1018','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1019','West-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1019','West-Directly'],'Area1019','Area1018'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1019','West-Directly'],string("Crusher's Quarters are West")),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1019','Up-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1019','Down-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1020")).
:-osimAssert(isa('Area1020','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(nameString('Area1020',string("Enterprise Security")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1020',string("Enterprise")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1020',string("Security")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1020',string("You're standing in the dimly lit Enterprise Security")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1020',string("Weapons lockers cover all of the walls, except along the northern wall, where a large glass window protects dozens of different phasors, blaster rifles, and other high tech weapons")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1020',string("Three long tables surrounded by chairs stretch across the room")),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1020','Indoors-IsolatedFromOutside'),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1020','SpaceInAHOC'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1019")).
:-osimAssert(isa('Area1019','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1020','West-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1020','West-Directly'],'Area1020','Area1019'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1020','West-Directly'],string("A corridor is West")),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1020','North-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1020','East-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1020','South-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1020','Up-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1020','Down-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1021")).
:-osimAssert(isa('Area1021','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(nameString('Area1021',string("Ten Forward")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1021',string("Ten")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1021',string("Forward")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1021',string("You're now in Ten Forward, the entertainment room of the Enterprise")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1021',string("The entire northern wall is covered with windows looking out into space, while two large wooden doors with the Starfleet insignia stamped on them face south")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1021',string("Many round metal tables are scattered around the room, surrounded by metal chairs")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1021',string("A long bar spans almost the entire length of the southern part of the room, and about two dozen bar stools are sitting in front of it")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1021',string("It's very noisy in here, due to all the talking and laughing")),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1021','Indoors-IsolatedFromOutside'),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1021','SpaceInAHOC'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1019")).
:-osimAssert(isa('Area1019','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1021','South-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1021','South-Directly'],'Area1021','Area1019'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1021','South-Directly'],string("A corridor is South")),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1021','North-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1021','East-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1021','West-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1021','Up-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1021','Down-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1022")).
:-osimAssert(isa('Area1022','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(nameString('Area1022',string("Shuttle Bay")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1022',string("Shuttle")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1022',string("Bay")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1022',string("You're in the main shuttle bay of the Enterprise")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1022',string("It's quite a large room, with a very high ceiling and a lot of floor space")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1022',string("You can see three different shuttle crafts sitting here, waiting to be flown")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1022',string("A large grey door leads into space")),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1022','Indoors-IsolatedFromOutside'),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1022','SpaceInAHOC'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1024")).
:-osimAssert(isa('Area1024','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1022','North-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1022','North-Directly'],'Area1022','Area1024'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1022','North-Directly'],string("A corridor is North")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1039")).
:-osimAssert(isa('Area1039','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1022','South-Directly'],'Doorway'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1022','South-Directly'],'Area1022','Area1039'),'*WorldStaticStateMt*').
:-osimAssert(nicknames(['BoundsOfDirectionFn','Area1022','South-Directly'],string("door")),'*WorldStaticStateMt*').
:-osimAssert(nicknames(['BoundsOfDirectionFn','Area1022','South-Directly'],string("grey")),'*WorldStaticStateMt*').
:-osimAssert(nicknames(['BoundsOfDirectionFn','Area1022','South-Directly'],string("doors")),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1022','East-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1022','West-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1022','Up-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1022','Down-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1023")).
:-osimAssert(isa('Area1023','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(nameString('Area1023',string("Troi's Quarters")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1023',string("Troi's")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1023',string("Quarters")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1023',string("You're in Counselor Deanna Troi's quarters")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1023',string("Several different paintings have been hung from the walls, and a small couch and a recliner are positioned around a coffee table")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1023',string("A neatly made bed is partially hidden behind a curtain at the northern part of the room")),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1023','Indoors-IsolatedFromOutside'),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1023','SpaceInAHOC'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1024")).
:-osimAssert(isa('Area1024','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1023','East-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1023','East-Directly'],'Area1023','Area1024'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1023','East-Directly'],string("A corridor is East")),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1023','North-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1023','South-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1023','West-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1023','Up-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1023','Down-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1024")).
:-osimAssert(isa('Area1024','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(nameString('Area1024',string("A Corridor")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1024',string("Corridor")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1024',string("You find yourself in the middle of a well lit corridor on the Enterprise")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1024',string("It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1024',string("You notice a tiny computer panel embedded into the wall")),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1024','Indoors-IsolatedFromOutside'),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1024','SpaceInAHOC'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1027")).
:-osimAssert(isa('Area1027','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1024','North-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1024','North-Directly'],'Area1024','Area1027'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1024','North-Directly'],string("The corridor continues North")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1025")).
:-osimAssert(isa('Area1025','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1024','East-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1024','East-Directly'],'Area1024','Area1025'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1024','East-Directly'],string("Worf's Quarters are East")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1022")).
:-osimAssert(isa('Area1022','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1024','South-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1024','South-Directly'],'Area1024','Area1022'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1024','South-Directly'],string("The shuttle bay is South")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1023")).
:-osimAssert(isa('Area1023','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1024','West-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1024','West-Directly'],'Area1024','Area1023'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1024','West-Directly'],string("Troi's Quarters are West")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1024-Object670")).
:-osimAssert(isa('Area1024-Object670','PartiallyTangible'),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1024-Object670',string("The panel says: <br> <br>*************************************************** <br>* * <br>* NCC-1701-D - 'ENTERPRISE' * <br>* * <br>* ***** * <br>* ********************** * <br>* *********************** _________ * <br>* ***** ***(___ ____( * <br>* *****  * * <br>* ********** * <br>* * <br>* You are currently on deck 3 * <br>* * <br>***************************************************")),'*WorldStaticStateMt*').
:-osimAssert('in-ContCompletely'('Area1024-Object670','Area1024'),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1024-Object670',string("control")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1024-Object670',string("panel")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1024-Object670',string("computer")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1024-Object670',string("screen")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1024-Object670',string("sign")),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1024','Up-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1024','Down-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1025")).
:-osimAssert(isa('Area1025','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(nameString('Area1025',string("Worf's Quarters")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1025',string("Worf's")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1025',string("Quarters")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1025',string("You're in Worf's quarters")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1025',string("A small table is sitting in the southeastern corner, and on it is a small potted plant")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1025',string("An impressive selection of Klingon weapons have been mounted on the northern wall, and a partition splits this room with Worf's bedroom to the east")),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1025','Indoors-IsolatedFromOutside'),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1025','SpaceInAHOC'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1024")).
:-osimAssert(isa('Area1024','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1025','West-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1025','West-Directly'],'Area1025','Area1024'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1025','West-Directly'],string("A corridor is West")),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1025','North-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1025','East-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1025','South-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1025','Up-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1025','Down-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1026")).
:-osimAssert(isa('Area1026','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(nameString('Area1026',string("Enterprise Gym")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1026',string("Enterprise")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1026',string("Gym")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1026',string("You emerge into the Enterprise gym")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1026',string("The room is quite large, with a soft grey floor")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1026',string("A set of lockers against the southern wall contain all of the necessary equipment needed for using the gym")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1026',string("A thick stack of mats have been piled high in one corner, which can be used for different activities")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1026',string("Captain Picard likes to come here to practice his fencing")),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1026','Indoors-IsolatedFromOutside'),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1026','SpaceInAHOC'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1027")).
:-osimAssert(isa('Area1027','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1026','East-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1026','East-Directly'],'Area1026','Area1027'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1026','East-Directly'],string("A corridor is East")),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1026','North-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1026','South-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1026','West-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1026','Up-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1026','Down-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1027")).
:-osimAssert(isa('Area1027','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(nameString('Area1027',string("A Corridor")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1027',string("Corridor")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1027',string("You find yourself in the middle of a well lit corridor on the Enterprise")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1027',string("It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape")),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1027','Indoors-IsolatedFromOutside'),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1027','SpaceInAHOC'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1030")).
:-osimAssert(isa('Area1030','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1027','North-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1027','North-Directly'],'Area1027','Area1030'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1027','North-Directly'],string("The corridor continues North")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1028")).
:-osimAssert(isa('Area1028','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1027','East-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1027','East-Directly'],'Area1027','Area1028'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1027','East-Directly'],string("Picard's Quarters are East")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1024")).
:-osimAssert(isa('Area1024','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1027','South-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1027','South-Directly'],'Area1027','Area1024'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1027','South-Directly'],string("The corridor continues South")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1026")).
:-osimAssert(isa('Area1026','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1027','West-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1027','West-Directly'],'Area1027','Area1026'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1027','West-Directly'],string("The Enterprise Gym is West")),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1027','Up-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1027','Down-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1028")).
:-osimAssert(isa('Area1028','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(nameString('Area1028',string("Picard's Quarters")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1028',string("Picard's")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1028',string("Quarters")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1028',string("You find yourself standing by the door of Captain Picard's quarters")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1028',string("He isn't very fond of visitors, but you decide to stay and have a look around")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1028',string("You can see several different ancient artifacts on tables and small pedestals, and a large wooden wardrobe is facing south")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1028',string("A comfortable looking recliner with a matching footrest sits beside the door, along with a bright reading lamp and end table")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1028',string("Two large windows offer a great view of space")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1028',string("A small partition at the northern part of the room contains Picard's sleeping area")),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1028','Indoors-IsolatedFromOutside'),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1028','SpaceInAHOC'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1027")).
:-osimAssert(isa('Area1027','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1028','West-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1028','West-Directly'],'Area1028','Area1027'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1028','West-Directly'],string("A corridor is West")),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1028','North-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1028','East-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1028','South-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1028','Up-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1028','Down-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1029")).
:-osimAssert(isa('Area1029','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(nameString('Area1029',string("Science Lab")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1029',string("Science")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1029',string("Lab")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1029',string("You're in the Enterprise science lab")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1029',string("A strange looking machine sits in the middle of the room, up on a slightly raised platform")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1029',string("It looks as though something(or someone) could be placed inside, hooked up to the multitude of wires and cables, and have scientific tests performed on it(or them)")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1029',string("A complex looking computer console is facing this machine")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1029',string("Around the rest of the room are counterops with with the odd computer terminal")),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1029','Indoors-IsolatedFromOutside'),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1029','SpaceInAHOC'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1030")).
:-osimAssert(isa('Area1030','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1029','East-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1029','East-Directly'],'Area1029','Area1030'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1029','East-Directly'],string("A corridor is East")),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1029','North-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1029','South-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1029','West-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1029','Up-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1029','Down-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1030")).
:-osimAssert(isa('Area1030','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(nameString('Area1030',string("A Corridor")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1030',string("Corridor")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1030',string("You find yourself in the middle of a well lit corridor on the Enterprise")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1030',string("It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape")),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1030','Indoors-IsolatedFromOutside'),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1030','SpaceInAHOC'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1032")).
:-osimAssert(isa('Area1032','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1030','North-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1030','North-Directly'],'Area1030','Area1032'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1030','North-Directly'],string("The Turbolift is North")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1031")).
:-osimAssert(isa('Area1031','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1030','East-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1030','East-Directly'],'Area1030','Area1031'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1030','East-Directly'],string("Empty Quarters are East")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1027")).
:-osimAssert(isa('Area1027','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1030','South-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1030','South-Directly'],'Area1030','Area1027'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1030','South-Directly'],string("The corridor continues South")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1029")).
:-osimAssert(isa('Area1029','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1030','West-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1030','West-Directly'],'Area1030','Area1029'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1030','West-Directly'],string("Empty Quarters are West")),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1030','Up-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1030','Down-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1031")).
:-osimAssert(isa('Area1031','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(nameString('Area1031',string("Cargo Bay 2")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1031',string("Cargo")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1031',string("Bay")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1031',string("You're in the cargo bay 2 of the Enterprise")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1031',string("It's quite a large room, with a very high ceiling and a lot of floor space")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1031',string("You can see several hundred plastic crates and barrels with the Starfleet insignia on them stacked right up to the ceiling")),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1031','Indoors-IsolatedFromOutside'),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1031','SpaceInAHOC'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1030")).
:-osimAssert(isa('Area1030','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1031','West-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1031','West-Directly'],'Area1031','Area1030'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1031','West-Directly'],string("A corridor is West")),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1031','North-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1031','East-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1031','South-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1031','Up-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1031','Down-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1032")).
:-osimAssert(isa('Area1032','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(nameString('Area1032',string("Turbolift")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1032',string("Turbolift")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1032',string("You're in the turbolift")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1032',string("The turbolift walls have been rounded off, making it in the shape of a tube")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1032',string("Several vertical rows of lights make this place very well lit")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1032',string("From here, you can access the other decks on the Enterprise")),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1032','Indoors-IsolatedFromOutside'),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1032','SpaceInAHOC'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1030")).
:-osimAssert(isa('Area1030','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1032','South-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1032','South-Directly'],'Area1032','Area1030'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1032','South-Directly'],string("A corridor leads South")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1033")).
:-osimAssert(isa('Area1033','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1032','Up-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1032','Up-Directly'],'Area1032','Area1033'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1032','Up-Directly'],string("The turbolift goes Up")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1011")).
:-osimAssert(isa('Area1011','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1032','Down-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1032','Down-Directly'],'Area1032','Area1011'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1032','Down-Directly'],string("The turbolift goes Down")),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1032','North-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1032','East-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1032','West-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1033")).
:-osimAssert(isa('Area1033','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(nameString('Area1033',string("Turbolift")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1033',string("Turbolift")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1033',string("You're in the turbolift")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1033',string("The turbolift walls have been rounded off, making it in the shape of a tube")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1033',string("Several vertical rows of lights make this place very well lit")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1033',string("From here, you can access the other decks on the Enterprise")),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1033','Indoors-IsolatedFromOutside'),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1033','SpaceInAHOC'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1034")).
:-osimAssert(isa('Area1034','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1033','Up-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1033','Up-Directly'],'Area1033','Area1034'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1033','Up-Directly'],string("The turbolift goes Up")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1032")).
:-osimAssert(isa('Area1032','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1033','Down-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1033','Down-Directly'],'Area1033','Area1032'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1033','Down-Directly'],string("The turbolift goes Down")),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1033','North-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1033','East-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1033','South-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1033','West-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1034")).
:-osimAssert(isa('Area1034','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(nameString('Area1034',string("Turbolift")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1034',string("Turbolift")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1034',string("You're in the turbolift")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1034',string("The turbolift walls have been rounded off, making it in the shape of a tube")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1034',string("Several vertical rows of lights make this place very well lit")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1034',string("From here, you can access the other decks on the Enterprise")),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1034','Indoors-IsolatedFromOutside'),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1034','SpaceInAHOC'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1036")).
:-osimAssert(isa('Area1036','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1034','Up-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1034','Up-Directly'],'Area1034','Area1036'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1034','Up-Directly'],string("The Main Bridge is Up")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1033")).
:-osimAssert(isa('Area1033','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1034','Down-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1034','Down-Directly'],'Area1034','Area1033'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1034','Down-Directly'],string("The turbolift goes Down")),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1034','North-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1034','East-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1034','South-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1034','West-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1035")).
:-osimAssert(isa('Area1035','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(nameString('Area1035',string("Picard's Ready Room")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1035',string("Picard's")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1035',string("Ready")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1035',string("Room")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1035',string("You're standing in Captain Picard's ready room")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1035',string("A long couch has been placed beside the door, while a large U shaped desk is located by the northern wall")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1035',string("A small computer screen is sitting on the desk, as well as several other papers and documents")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1035',string("A single high window beside the desk looks into space, and a fish tank is located in the northwestern corner of the room")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1035',string("This is where the Captain makes all of his important decisions")),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1035','Indoors-IsolatedFromOutside'),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1035','SpaceInAHOC'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1038")).
:-osimAssert(isa('Area1038','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1035','East-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1035','East-Directly'],'Area1035','Area1038'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1035','East-Directly'],string("The Main Bridge - Lower Half is East")),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1035','North-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1035','South-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1035','West-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1035','Up-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1035','Down-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1036")).
:-osimAssert(isa('Area1036','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(nameString('Area1036',string("Main Bridge - Upper Half")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1036',string("Main")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1036',string("Bridge")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1036',string("Upper")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1036',string("Half")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1036',string("You find yourself on the upper half of the main bridge of the USS Enterprise")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1036',string("Directly in front of you is a thick railing that contains many different computer panels used for the tactical systems of the ship")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1036',string("The entire southern wall is covered with computer consoles, where the ship's main systems are controlled")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1036',string("Two small curved ramps on either side of the room lead north to the lower part of the bridge, and a large circular skylight shows the space outside the ship")),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1036','Indoors-IsolatedFromOutside'),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1036','SpaceInAHOC'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1038")).
:-osimAssert(isa('Area1038','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1036','North-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1036','North-Directly'],'Area1036','Area1038'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1036','North-Directly'],string("The Main Bridge - Lower Half is North")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1037")).
:-osimAssert(isa('Area1037','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1036','East-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1036','East-Directly'],'Area1036','Area1037'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1036','East-Directly'],string("The Conference Room is East")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1034")).
:-osimAssert(isa('Area1034','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1036','Down-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1036','Down-Directly'],'Area1036','Area1034'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1036','Down-Directly'],string("The Turbolift is Down")),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1036','South-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1036','West-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1036','Up-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1037")).
:-osimAssert(isa('Area1037','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(nameString('Area1037',string("Conference Room")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1037',string("Conference")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1037',string("Room")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1037',string("You're in the conference room of the Enterprise")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1037',string("A large glass rectangular table sits in the middle of the room, surrounded by about a dozen comfortable looking office chairs")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1037',string("The entire eastern wall is covered with windows, looking out into space")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1037',string("This is where the senior officers of the Enterprise meet and discuss important issues")),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1037','Indoors-IsolatedFromOutside'),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1037','SpaceInAHOC'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1036")).
:-osimAssert(isa('Area1036','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1037','West-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1037','West-Directly'],'Area1037','Area1036'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1037','West-Directly'],string("The Main Bridge - Upper Half is West")),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1037','North-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1037','East-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1037','South-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1037','Up-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1037','Down-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1038")).
:-osimAssert(isa('Area1038','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(nameString('Area1038',string("Main Bridge - Lower Half")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1038',string("Main")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1038',string("Bridge")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1038',string("Lower")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1038',string("Half")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1038',string("You find yourself on the lower half of the main bridge of the USS Enterprise")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1038',string("An enormous view screen covers almost the entire northern wall, and is currently displaying a view of the stars rushing by")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1038',string("Three large chairs in the northern part of the room, in front of the railing, face the screen")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1038',string("This is where the Captain, Commander Riker, and Counselor Troi sit")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1038',string("Two computer consoles with built in chairs rest about ten feet in front of the chairs, also facing the view screen")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1038',string("This is where the ship's pilot and information officer sit")),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1038','Indoors-IsolatedFromOutside'),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1038','SpaceInAHOC'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1036")).
:-osimAssert(isa('Area1036','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1038','South-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1038','South-Directly'],'Area1038','Area1036'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1038','South-Directly'],string("The Main Bridge - Upper Half is South")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1035")).
:-osimAssert(isa('Area1035','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1038','West-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1038','West-Directly'],'Area1038','Area1035'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1038','West-Directly'],string("The Captain's Ready Room is West")),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1038','North-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1038','East-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1038','Up-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1038','Down-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1039")).
:-osimAssert(isa('Area1039','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(nameString('Area1039',string("Outer Space by the Enterprise")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1039',string("Outer")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1039',string("Space")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1039',string("Enterprise")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1039',string("You're floating in outer space right beside the USS Enterprise")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1039',string("You can see stars in every direction, which provide the only light here")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1039',string("You feel very cold")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1039',string("A large grey door leads into the Enterprise's Shuttle Bay")),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1039','FreeSpaceContent'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1022")).
:-osimAssert(isa('Area1022','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1039','North-Directly'],'Doorway'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1039','North-Directly'],'Area1039','Area1022'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1039','North-Directly'],string("The Shuttle Bay is North")),'*WorldStaticStateMt*').
:-osimAssert(nicknames(['BoundsOfDirectionFn','Area1039','North-Directly'],string("door")),'*WorldStaticStateMt*').
:-osimAssert(nicknames(['BoundsOfDirectionFn','Area1039','North-Directly'],string("doors")),'*WorldStaticStateMt*').
:-osimAssert(nicknames(['BoundsOfDirectionFn','Area1039','North-Directly'],string("grey")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1040")).
:-osimAssert(isa('Area1040','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1039','Up-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1039','Up-Directly'],'Area1039','Area1040'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1039','Up-Directly'],string("Outer Space is Up")),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1039','East-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1039','South-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1039','West-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1039','Down-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1040")).
:-osimAssert(isa('Area1040','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(nameString('Area1040',string("Outer Space")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1040',string("Outer")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1040',string("Space")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1040',string("You're floating in outer space right above the USS Enterprise")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1040',string("You can see stars in every direction, which provide the only light here")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1040',string("You feel very cold")),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1040','FreeSpaceContent'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1041")).
:-osimAssert(isa('Area1041','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1040','Up-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1040','Up-Directly'],'Area1040','Area1041'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1040','Up-Directly'],string("Outer Space is Up")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1039")).
:-osimAssert(isa('Area1039','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1040','Down-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1040','Down-Directly'],'Area1040','Area1039'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1040','Down-Directly'],string("Outer Space is Down")),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1040','North-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1040','East-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1040','South-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1040','West-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1041")).
:-osimAssert(isa('Area1041','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(nameString('Area1041',string("Outer Space")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1041',string("Outer")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1041',string("Space")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1041',string("You're floating in outer space right above the USS Enterprise")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1041',string("You can see stars in every direction, which provide the only light here")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1041',string("You feel very cold")),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1041','FreeSpaceContent'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area-1")).
:-osimAssert(isa('Area-1','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1041','Up-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1041','Up-Directly'],'Area1041','Area1041'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1041','Up-Directly'],string("* The Galaxy is Up")),'*WorldStaticStateMt*').
:-osimAssert(nicknames(['BoundsOfDirectionFn','Area1041','Up-Directly'],string("*")),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1040")).
:-osimAssert(isa('Area1040','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1041','Down-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1041','Down-Directly'],'Area1041','Area1040'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1041','Down-Directly'],string("Outer Space is Down")),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1041','North-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1041','East-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1041','South-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1041','West-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1042")).
:-osimAssert(isa('Area1042','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(nameString('Area1042',string("Transporter Beam")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1042',string("Transporter")),'*WorldStaticStateMt*').
:-osimAssert(nicknames('Area1042',string("Beam")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1042',string("You find yourself in a transporter beam")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1042',string("All you can see is blue flashing light")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1042',string("It feels as though your body is racing around at high speeds")),'*WorldStaticStateMt*').
:-osimAssert(definiteDescriptions('Area1042',string("As you try to look down at your body, you realize that there's nothing there!")),'*WorldStaticStateMt*').
:-osimAssert(isa('Area1042','FreeSpaceContent'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area4075")).
:-osimAssert(isa('Area4075','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1042','North-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1042','North-Directly'],'Area1042','Area4075'),'*WorldStaticStateMt*').
'find-or-create-constant'(string("Area1006")).
:-osimAssert(isa('Area1006','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1042','South-Directly'],'OpenPortal'),'*WorldStaticStateMt*').
:-osimAssert(pathBetween(['BoundsOfDirectionFn','Area1042','South-Directly'],'Area1042','Area1006'),'*WorldStaticStateMt*').
:-osimAssert(nameString(['BoundsOfDirectionFn','Area1042','South-Directly'],string("The Transporter Room is South")),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1042','East-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1042','West-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1042','Up-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1042','Down-Directly'],'Wall-GenericBarrier'),'*WorldStaticStateMt*').
%% pipequoted("load file '10.wld.kif'").
%% pipequoted("load file '10.zon'").
%% pipequoted("; Sourcing C:OpenSimsrcdaxmooworlds10.zon").
'find-or-create-constant'(string("NpcCol1013")).
:-osimAssert(isa('NpcCol1013','BPVAgentType'),'*WorldVocabularyMt*').
:-osimAssert(genls('NpcCol1013','Agent-Generic'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("NpcCol1013-Alexander671")).
'find-or-create-constant'(string("NpcCol1013")).
:-osimAssert(isa('NpcCol1013-Alexander671','NpcCol1013'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('NpcCol1013-Alexander671',string("Alexander")),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("Area1025")).
:-osimAssert(isa('Area1025','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert('in-ContCompletely'('NpcCol1013-Alexander671','Area1025'),'*WorldCurrentStateMt*').

:-osimAssert(isa('NpcCol1001','BPVAgentType'),'*WorldVocabularyMt*').
:-osimAssert(genls('NpcCol1001','Agent-Generic'),'*WorldVocabularyMt*').

%% pipequoted(";(find-or-create-constant \"cyc_bot_1\")").
:-osimAssert(isa(['OSimItemFn',string("cyc_bot_1")],'NpcCol1001'),'*WorldCurrentStateMt*').
:-osimAssert(nameString(['OSimItemFn',string("cyc_bot_1")],string("Data")),'*WorldCurrentStateMt*').
:-osimAssert(nameString(['OSimItemFn',string("cyc_bot_1")],string("CycLBot")),'*WorldCurrentStateMt*').
:-osimAssert(nameString(['OSimItemFn',string("cyc_bot_1")],string("CycBot")),'*WorldCurrentStateMt*').
:-osimAssert(nameString(['OSimItemFn',string("cyc_bot_1")],string("CycBot1")),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("Area1029")).
:-osimAssert(isa('Area1029','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert('in-ContCompletely'(['OSimItemFn',string("cyc_bot_1")],'Area1010'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1005")).
:-osimAssert(isa('ArtifactCol1005','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1005','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1005-Boots673")).
'find-or-create-constant'(string("ArtifactCol1005")).
:-osimAssert(isa('ArtifactCol1005-Boots673','ArtifactCol1005'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1005-Boots673',string("Boots")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1005-Boots673',['The',['BodyPartCollectionFn',['OSimItemFn',string("cyc_bot_1")],'Foot-AnimalBodyPart']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1006")).
:-osimAssert(isa('ArtifactCol1006','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1006','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1006-Comm-Badge674")).
'find-or-create-constant'(string("ArtifactCol1006")).
:-osimAssert(isa('ArtifactCol1006-Comm-Badge674','ArtifactCol1006'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1006-Comm-Badge674',string("Comm Badge")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1006-Comm-Badge674',['The',['BodyPartCollectionFn',['OSimItemFn',string("cyc_bot_1")],'Neck-AnimalBodyPart']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1003")).
:-osimAssert(isa('ArtifactCol1003','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1003','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1003-Gold-Uniform675")).
'find-or-create-constant'(string("ArtifactCol1003")).
:-osimAssert(isa('ArtifactCol1003-Gold-Uniform675','ArtifactCol1003'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1003-Gold-Uniform675',string("Gold Uniform")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1003-Gold-Uniform675',['The',['BodyPartCollectionFn',['OSimItemFn',string("cyc_bot_1")],'Torso']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1000")).
:-osimAssert(isa('ArtifactCol1000','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1000','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1000-Phaser676")).
'find-or-create-constant'(string("ArtifactCol1000")).
:-osimAssert(isa('ArtifactCol1000-Phaser676','ArtifactCol1000'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1000-Phaser676',string("Phaser")),'*WorldCurrentStateMt*').
:-osimAssert(possesses(['OSimItemFn',string("cyc_bot_1")],'ArtifactCol1000-Phaser676'),'*WorldCurrentStateMt*').

%% pipequoted(";(find-or-create-constant \"player1\")").
:-osimAssert(isa(['OSimItemFn',string("player1")],['OSimClassFn',string("player_osimmarine_mp")]),'*WorldCurrentStateMt*').
:-osimAssert(nameString(['OSimItemFn',string("player1")],string("Player")),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("Area1029")).
:-osimAssert(isa('Area1029','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert('in-ContCompletely'(['OSimItemFn',string("player1")],'Area1000'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1005")).
:-osimAssert(isa('ArtifactCol1005','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1005','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1005-Boots773")).
'find-or-create-constant'(string("ArtifactCol1005")).
:-osimAssert(isa('ArtifactCol1005-Boots773','ArtifactCol1005'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1005-Boots773',string("Boots")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1005-Boots773',['The',['BodyPartCollectionFn',['OSimItemFn',string("player1")],'Foot-AnimalBodyPart']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1006")).
:-osimAssert(isa('ArtifactCol1006','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1006','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1006-Comm-Badge774")).
'find-or-create-constant'(string("ArtifactCol1006")).
:-osimAssert(isa('ArtifactCol1006-Comm-Badge774','ArtifactCol1006'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1006-Comm-Badge774',string("Comm Badge")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1006-Comm-Badge774',['The',['BodyPartCollectionFn',['OSimItemFn',string("player1")],'Neck-AnimalBodyPart']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1003")).
:-osimAssert(isa('ArtifactCol1003','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1003','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1003-Gold-Uniform775")).
'find-or-create-constant'(string("ArtifactCol1003")).
:-osimAssert(isa('ArtifactCol1003-Gold-Uniform775','ArtifactCol1003'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1003-Gold-Uniform775',string("Gold Uniform")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1003-Gold-Uniform775',['The',['BodyPartCollectionFn',['OSimItemFn',string("player1")],'Torso']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1000")).
:-osimAssert(isa('ArtifactCol1000','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1000','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1000-Phaser776")).
'find-or-create-constant'(string("ArtifactCol1000")).
:-osimAssert(isa('ArtifactCol1000-Phaser776','ArtifactCol1000'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1000-Phaser776',string("Phaser")),'*WorldCurrentStateMt*').
:-osimAssert(possesses(['OSimItemFn',string("player1")],'ArtifactCol1000-Phaser776'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("NpcCol1003")).
:-osimAssert(isa('NpcCol1003','BPVAgentType'),'*WorldVocabularyMt*').
:-osimAssert(genls('NpcCol1003','Agent-Generic'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("NpcCol1003-Dr-Crusher677")).
'find-or-create-constant'(string("NpcCol1003")).
:-osimAssert(isa('NpcCol1003-Dr-Crusher677','NpcCol1003'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('NpcCol1003-Dr-Crusher677',string("Dr. Crusher")),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("Area1015")).
:-osimAssert(isa('Area1015','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert('in-ContCompletely'('NpcCol1003-Dr-Crusher677','Area1015'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1005")).
:-osimAssert(isa('ArtifactCol1005','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1005','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1005-Boots678")).
'find-or-create-constant'(string("ArtifactCol1005")).
:-osimAssert(isa('ArtifactCol1005-Boots678','ArtifactCol1005'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1005-Boots678',string("Boots")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1005-Boots678',['The',['BodyPartCollectionFn','NpcCol1003-Dr-Crusher677','Foot-AnimalBodyPart']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1006")).
:-osimAssert(isa('ArtifactCol1006','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1006','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1006-Comm-Badge679")).
'find-or-create-constant'(string("ArtifactCol1006")).
:-osimAssert(isa('ArtifactCol1006-Comm-Badge679','ArtifactCol1006'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1006-Comm-Badge679',string("Comm Badge")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1006-Comm-Badge679',['The',['BodyPartCollectionFn','NpcCol1003-Dr-Crusher677','Neck-AnimalBodyPart']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1004")).
:-osimAssert(isa('ArtifactCol1004','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1004','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1004-Blue-Uniform680")).
'find-or-create-constant'(string("ArtifactCol1004")).
:-osimAssert(isa('ArtifactCol1004-Blue-Uniform680','ArtifactCol1004'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1004-Blue-Uniform680',string("Blue Uniform")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1004-Blue-Uniform680',['The',['BodyPartCollectionFn','NpcCol1003-Dr-Crusher677','Torso']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1009")).
:-osimAssert(isa('ArtifactCol1009','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1009','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1009-Medical-Tricorder681")).
'find-or-create-constant'(string("ArtifactCol1009")).
:-osimAssert(isa('ArtifactCol1009-Medical-Tricorder681','ArtifactCol1009'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1009-Medical-Tricorder681',string("Medical Tricorder")),'*WorldCurrentStateMt*').
:-osimAssert(possesses('NpcCol1003-Dr-Crusher677','ArtifactCol1009-Medical-Tricorder681'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1009")).
:-osimAssert(isa('ArtifactCol1009','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1009','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1009-Medical-Tricorder682")).
'find-or-create-constant'(string("ArtifactCol1009")).
:-osimAssert(isa('ArtifactCol1009-Medical-Tricorder682','ArtifactCol1009'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1009-Medical-Tricorder682',string("Medical Tricorder")),'*WorldCurrentStateMt*').
:-osimAssert(possesses('NpcCol1003-Dr-Crusher677','ArtifactCol1009-Medical-Tricorder682'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1009")).
:-osimAssert(isa('ArtifactCol1009','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1009','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1009-Medical-Tricorder683")).
'find-or-create-constant'(string("ArtifactCol1009")).
:-osimAssert(isa('ArtifactCol1009-Medical-Tricorder683','ArtifactCol1009'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1009-Medical-Tricorder683',string("Medical Tricorder")),'*WorldCurrentStateMt*').
:-osimAssert(possesses('NpcCol1003-Dr-Crusher677','ArtifactCol1009-Medical-Tricorder683'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("NpcCol1000")).
:-osimAssert(isa('NpcCol1000','BPVAgentType'),'*WorldVocabularyMt*').
:-osimAssert(genls('NpcCol1000','Agent-Generic'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("NpcCol1000-Geordi684")).
'find-or-create-constant'(string("NpcCol1000")).
:-osimAssert(isa('NpcCol1000-Geordi684','NpcCol1000'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('NpcCol1000-Geordi684',string("Geordi")),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("Area1000")).
:-osimAssert(isa('Area1000','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert('in-ContCompletely'('NpcCol1000-Geordi684','Area1000'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1005")).
:-osimAssert(isa('ArtifactCol1005','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1005','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1005-Boots685")).
'find-or-create-constant'(string("ArtifactCol1005")).
:-osimAssert(isa('ArtifactCol1005-Boots685','ArtifactCol1005'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1005-Boots685',string("Boots")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1005-Boots685',['The',['BodyPartCollectionFn','NpcCol1000-Geordi684','Foot-AnimalBodyPart']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1006")).
:-osimAssert(isa('ArtifactCol1006','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1006','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1006-Comm-Badge686")).
'find-or-create-constant'(string("ArtifactCol1006")).
:-osimAssert(isa('ArtifactCol1006-Comm-Badge686','ArtifactCol1006'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1006-Comm-Badge686',string("Comm Badge")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1006-Comm-Badge686',['The',['BodyPartCollectionFn','NpcCol1000-Geordi684','Neck-AnimalBodyPart']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1003")).
:-osimAssert(isa('ArtifactCol1003','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1003','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1003-Gold-Uniform687")).
'find-or-create-constant'(string("ArtifactCol1003")).
:-osimAssert(isa('ArtifactCol1003-Gold-Uniform687','ArtifactCol1003'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1003-Gold-Uniform687',string("Gold Uniform")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1003-Gold-Uniform687',['The',['BodyPartCollectionFn','NpcCol1000-Geordi684','Torso']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1008")).
:-osimAssert(isa('ArtifactCol1008','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1008','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1008-VISOR688")).
'find-or-create-constant'(string("ArtifactCol1008")).
:-osimAssert(isa('ArtifactCol1008-VISOR688','ArtifactCol1008'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1008-VISOR688',string("VISOR")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1008-VISOR688',['The',['BodyPartCollectionFn','NpcCol1000-Geordi684','FaceOfAnimal']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("NpcCol1007")).
:-osimAssert(isa('NpcCol1007','BPVAgentType'),'*WorldVocabularyMt*').
:-osimAssert(genls('NpcCol1007','Agent-Generic'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("NpcCol1007-Guinan689")).
'find-or-create-constant'(string("NpcCol1007")).
:-osimAssert(isa('NpcCol1007-Guinan689','NpcCol1007'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('NpcCol1007-Guinan689',string("Guinan")),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("Area1021")).
:-osimAssert(isa('Area1021','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert('in-ContCompletely'('NpcCol1007-Guinan689','Area1021'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1020")).
:-osimAssert(isa('ArtifactCol1020','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1020','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1020-Tea690")).
'find-or-create-constant'(string("ArtifactCol1020")).
:-osimAssert(isa('ArtifactCol1020-Tea690','ArtifactCol1020'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1020-Tea690',string("Tea")),'*WorldCurrentStateMt*').
:-osimAssert(possesses('NpcCol1007-Guinan689','ArtifactCol1020-Tea690'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1021")).
:-osimAssert(isa('ArtifactCol1021','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1021','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1021-Synthehol691")).
'find-or-create-constant'(string("ArtifactCol1021")).
:-osimAssert(isa('ArtifactCol1021-Synthehol691','ArtifactCol1021'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1021-Synthehol691',string("Synthehol")),'*WorldCurrentStateMt*').
:-osimAssert(possesses('NpcCol1007-Guinan689','ArtifactCol1021-Synthehol691'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1022")).
:-osimAssert(isa('ArtifactCol1022','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1022','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1022-Ferengi-Ale692")).
'find-or-create-constant'(string("ArtifactCol1022")).
:-osimAssert(isa('ArtifactCol1022-Ferengi-Ale692','ArtifactCol1022'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1022-Ferengi-Ale692',string("Ferengi Ale")),'*WorldCurrentStateMt*').
:-osimAssert(possesses('NpcCol1007-Guinan689','ArtifactCol1022-Ferengi-Ale692'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1023")).
:-osimAssert(isa('ArtifactCol1023','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1023','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1023-Romulan-Whisky693")).
'find-or-create-constant'(string("ArtifactCol1023")).
:-osimAssert(isa('ArtifactCol1023-Romulan-Whisky693','ArtifactCol1023'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1023-Romulan-Whisky693',string("Romulan Whisky")),'*WorldCurrentStateMt*').
:-osimAssert(possesses('NpcCol1007-Guinan689','ArtifactCol1023-Romulan-Whisky693'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1024")).
:-osimAssert(isa('ArtifactCol1024','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1024','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1024-Lemonade-Prune-Juice694")).
'find-or-create-constant'(string("ArtifactCol1024")).
:-osimAssert(isa('ArtifactCol1024-Lemonade-Prune-Juice694','ArtifactCol1024'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1024-Lemonade-Prune-Juice694',string("Lemonade 'Prune Juice'")),'*WorldCurrentStateMt*').
:-osimAssert(possesses('NpcCol1007-Guinan689','ArtifactCol1024-Lemonade-Prune-Juice694'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1025")).
:-osimAssert(isa('ArtifactCol1025','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1025','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1025-Vulcan-Beer695")).
'find-or-create-constant'(string("ArtifactCol1025")).
:-osimAssert(isa('ArtifactCol1025-Vulcan-Beer695','ArtifactCol1025'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1025-Vulcan-Beer695',string("Vulcan Beer")),'*WorldCurrentStateMt*').
:-osimAssert(possesses('NpcCol1007-Guinan689','ArtifactCol1025-Vulcan-Beer695'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("NpcCol1008")).
:-osimAssert(isa('NpcCol1008','BPVAgentType'),'*WorldVocabularyMt*').
:-osimAssert(genls('NpcCol1008','Agent-Generic'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("NpcCol1008-OBrien696")).
'find-or-create-constant'(string("NpcCol1008")).
:-osimAssert(isa('NpcCol1008-OBrien696','NpcCol1008'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('NpcCol1008-OBrien696',string("O'Brien")),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("Area1006")).
:-osimAssert(isa('Area1006','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert('in-ContCompletely'('NpcCol1008-OBrien696','Area1006'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1005")).
:-osimAssert(isa('ArtifactCol1005','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1005','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1005-Boots697")).
'find-or-create-constant'(string("ArtifactCol1005")).
:-osimAssert(isa('ArtifactCol1005-Boots697','ArtifactCol1005'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1005-Boots697',string("Boots")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1005-Boots697',['The',['BodyPartCollectionFn','NpcCol1008-OBrien696','Foot-AnimalBodyPart']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1006")).
:-osimAssert(isa('ArtifactCol1006','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1006','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1006-Comm-Badge698")).
'find-or-create-constant'(string("ArtifactCol1006")).
:-osimAssert(isa('ArtifactCol1006-Comm-Badge698','ArtifactCol1006'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1006-Comm-Badge698',string("Comm Badge")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1006-Comm-Badge698',['The',['BodyPartCollectionFn','NpcCol1008-OBrien696','Neck-AnimalBodyPart']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1003")).
:-osimAssert(isa('ArtifactCol1003','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1003','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1003-Gold-Uniform699")).
'find-or-create-constant'(string("ArtifactCol1003")).
:-osimAssert(isa('ArtifactCol1003-Gold-Uniform699','ArtifactCol1003'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1003-Gold-Uniform699',string("Gold Uniform")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1003-Gold-Uniform699',['The',['BodyPartCollectionFn','NpcCol1008-OBrien696','Torso']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1000")).
:-osimAssert(isa('ArtifactCol1000','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1000','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1000-Phaser700")).
'find-or-create-constant'(string("ArtifactCol1000")).
:-osimAssert(isa('ArtifactCol1000-Phaser700','ArtifactCol1000'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1000-Phaser700',string("Phaser")),'*WorldCurrentStateMt*').
:-osimAssert(possesses('NpcCol1008-OBrien696','ArtifactCol1000-Phaser700'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("NpcCol1006")).
:-osimAssert(isa('NpcCol1006','BPVAgentType'),'*WorldVocabularyMt*').
:-osimAssert(genls('NpcCol1006','Agent-Generic'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("NpcCol1006-Picard701")).
'find-or-create-constant'(string("NpcCol1006")).
:-osimAssert(isa('NpcCol1006-Picard701','NpcCol1006'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('NpcCol1006-Picard701',string("Picard")),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("Area1035")).
:-osimAssert(isa('Area1035','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert('in-ContCompletely'('NpcCol1006-Picard701','Area1035'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1005")).
:-osimAssert(isa('ArtifactCol1005','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1005','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1005-Boots702")).
'find-or-create-constant'(string("ArtifactCol1005")).
:-osimAssert(isa('ArtifactCol1005-Boots702','ArtifactCol1005'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1005-Boots702',string("Boots")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1005-Boots702',['The',['BodyPartCollectionFn','NpcCol1006-Picard701','Foot-AnimalBodyPart']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1006")).
:-osimAssert(isa('ArtifactCol1006','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1006','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1006-Comm-Badge703")).
'find-or-create-constant'(string("ArtifactCol1006")).
:-osimAssert(isa('ArtifactCol1006-Comm-Badge703','ArtifactCol1006'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1006-Comm-Badge703',string("Comm Badge")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1006-Comm-Badge703',['The',['BodyPartCollectionFn','NpcCol1006-Picard701','Neck-AnimalBodyPart']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1002")).
:-osimAssert(isa('ArtifactCol1002','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1002','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1002-Red-Uniform704")).
'find-or-create-constant'(string("ArtifactCol1002")).
:-osimAssert(isa('ArtifactCol1002-Red-Uniform704','ArtifactCol1002'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1002-Red-Uniform704',string("Red Uniform")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1002-Red-Uniform704',['The',['BodyPartCollectionFn','NpcCol1006-Picard701','Torso']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1001")).
:-osimAssert(isa('ArtifactCol1001','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1001','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1001-5-Phaser-Rifle705")).
'find-or-create-constant'(string("ArtifactCol1001")).
:-osimAssert(isa('ArtifactCol1001-5-Phaser-Rifle705','ArtifactCol1001'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1001-5-Phaser-Rifle705',string("5 Phaser Rifle")),'*WorldCurrentStateMt*').
:-osimAssert(possesses('NpcCol1006-Picard701','ArtifactCol1001-5-Phaser-Rifle705'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1011")).
:-osimAssert(isa('ArtifactCol1011','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1011','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1011-5-Picards-Flute706")).
'find-or-create-constant'(string("ArtifactCol1011")).
:-osimAssert(isa('ArtifactCol1011-5-Picards-Flute706','ArtifactCol1011'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1011-5-Picards-Flute706',string("5 Picard's Flute")),'*WorldCurrentStateMt*').
:-osimAssert(possesses('NpcCol1006-Picard701','ArtifactCol1011-5-Picards-Flute706'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("NpcCol1005")).
:-osimAssert(isa('NpcCol1005','BPVAgentType'),'*WorldVocabularyMt*').
:-osimAssert(genls('NpcCol1005','Agent-Generic'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("NpcCol1005-Riker707")).
'find-or-create-constant'(string("NpcCol1005")).
:-osimAssert(isa('NpcCol1005-Riker707','NpcCol1005'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('NpcCol1005-Riker707',string("Riker")),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("Area1036")).
:-osimAssert(isa('Area1036','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert('in-ContCompletely'('NpcCol1005-Riker707','Area1036'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1005")).
:-osimAssert(isa('ArtifactCol1005','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1005','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1005-Boots708")).
'find-or-create-constant'(string("ArtifactCol1005")).
:-osimAssert(isa('ArtifactCol1005-Boots708','ArtifactCol1005'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1005-Boots708',string("Boots")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1005-Boots708',['The',['BodyPartCollectionFn','NpcCol1005-Riker707','Foot-AnimalBodyPart']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1006")).
:-osimAssert(isa('ArtifactCol1006','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1006','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1006-Comm-Badge709")).
'find-or-create-constant'(string("ArtifactCol1006")).
:-osimAssert(isa('ArtifactCol1006-Comm-Badge709','ArtifactCol1006'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1006-Comm-Badge709',string("Comm Badge")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1006-Comm-Badge709',['The',['BodyPartCollectionFn','NpcCol1005-Riker707','Neck-AnimalBodyPart']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1002")).
:-osimAssert(isa('ArtifactCol1002','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1002','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1002-Red-Uniform710")).
'find-or-create-constant'(string("ArtifactCol1002")).
:-osimAssert(isa('ArtifactCol1002-Red-Uniform710','ArtifactCol1002'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1002-Red-Uniform710',string("Red Uniform")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1002-Red-Uniform710',['The',['BodyPartCollectionFn','NpcCol1005-Riker707','Torso']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1012")).
:-osimAssert(isa('ArtifactCol1012','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1012','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1012-Trombone711")).
'find-or-create-constant'(string("ArtifactCol1012")).
:-osimAssert(isa('ArtifactCol1012-Trombone711','ArtifactCol1012'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1012-Trombone711',string("Trombone")),'*WorldCurrentStateMt*').
:-osimAssert(possesses('NpcCol1005-Riker707','ArtifactCol1012-Trombone711'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("NpcCol1004")).
:-osimAssert(isa('NpcCol1004','BPVAgentType'),'*WorldVocabularyMt*').
:-osimAssert(genls('NpcCol1004','Agent-Generic'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("NpcCol1004-Troi712")).
'find-or-create-constant'(string("NpcCol1004")).
:-osimAssert(isa('NpcCol1004-Troi712','NpcCol1004'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('NpcCol1004-Troi712',string("Troi")),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("Area1007")).
:-osimAssert(isa('Area1007','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert('in-ContCompletely'('NpcCol1004-Troi712','Area1007'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1005")).
:-osimAssert(isa('ArtifactCol1005','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1005','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1005-Boots713")).
'find-or-create-constant'(string("ArtifactCol1005")).
:-osimAssert(isa('ArtifactCol1005-Boots713','ArtifactCol1005'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1005-Boots713',string("Boots")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1005-Boots713',['The',['BodyPartCollectionFn','NpcCol1004-Troi712','Foot-AnimalBodyPart']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1006")).
:-osimAssert(isa('ArtifactCol1006','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1006','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1006-Comm-Badge714")).
'find-or-create-constant'(string("ArtifactCol1006")).
:-osimAssert(isa('ArtifactCol1006-Comm-Badge714','ArtifactCol1006'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1006-Comm-Badge714',string("Comm Badge")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1006-Comm-Badge714',['The',['BodyPartCollectionFn','NpcCol1004-Troi712','Neck-AnimalBodyPart']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1004")).
:-osimAssert(isa('ArtifactCol1004','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1004','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1004-Blue-Uniform715")).
'find-or-create-constant'(string("ArtifactCol1004")).
:-osimAssert(isa('ArtifactCol1004-Blue-Uniform715','ArtifactCol1004'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1004-Blue-Uniform715',string("Blue Uniform")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1004-Blue-Uniform715',['The',['BodyPartCollectionFn','NpcCol1004-Troi712','Torso']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("NpcCol1009")).
:-osimAssert(isa('NpcCol1009','BPVAgentType'),'*WorldVocabularyMt*').
:-osimAssert(genls('NpcCol1009','Agent-Generic'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("NpcCol1009-Wesley716")).
'find-or-create-constant'(string("NpcCol1009")).
:-osimAssert(isa('NpcCol1009-Wesley716','NpcCol1009'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('NpcCol1009-Wesley716',string("Wesley")),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("Area1016")).
:-osimAssert(isa('Area1016','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert('in-ContCompletely'('NpcCol1009-Wesley716','Area1016'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1005")).
:-osimAssert(isa('ArtifactCol1005','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1005','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1005-Boots717")).
'find-or-create-constant'(string("ArtifactCol1005")).
:-osimAssert(isa('ArtifactCol1005-Boots717','ArtifactCol1005'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1005-Boots717',string("Boots")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1005-Boots717',['The',['BodyPartCollectionFn','NpcCol1009-Wesley716','Foot-AnimalBodyPart']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1006")).
:-osimAssert(isa('ArtifactCol1006','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1006','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1006-Comm-Badge718")).
'find-or-create-constant'(string("ArtifactCol1006")).
:-osimAssert(isa('ArtifactCol1006-Comm-Badge718','ArtifactCol1006'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1006-Comm-Badge718',string("Comm Badge")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1006-Comm-Badge718',['The',['BodyPartCollectionFn','NpcCol1009-Wesley716','Neck-AnimalBodyPart']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1002")).
:-osimAssert(isa('ArtifactCol1002','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1002','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1002-Red-Uniform719")).
'find-or-create-constant'(string("ArtifactCol1002")).
:-osimAssert(isa('ArtifactCol1002-Red-Uniform719','ArtifactCol1002'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1002-Red-Uniform719',string("Red Uniform")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1002-Red-Uniform719',['The',['BodyPartCollectionFn','NpcCol1009-Wesley716','Torso']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("NpcCol1002")).
:-osimAssert(isa('NpcCol1002','BPVAgentType'),'*WorldVocabularyMt*').
:-osimAssert(genls('NpcCol1002','Agent-Generic'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("NpcCol1002-Worf720")).
'find-or-create-constant'(string("NpcCol1002")).
:-osimAssert(isa('NpcCol1002-Worf720','NpcCol1002'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('NpcCol1002-Worf720',string("Worf")),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("Area1025")).
:-osimAssert(isa('Area1025','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert('in-ContCompletely'('NpcCol1002-Worf720','Area1025'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1005")).
:-osimAssert(isa('ArtifactCol1005','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1005','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1005-Boots721")).
'find-or-create-constant'(string("ArtifactCol1005")).
:-osimAssert(isa('ArtifactCol1005-Boots721','ArtifactCol1005'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1005-Boots721',string("Boots")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1005-Boots721',['The',['BodyPartCollectionFn','NpcCol1002-Worf720','Foot-AnimalBodyPart']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1006")).
:-osimAssert(isa('ArtifactCol1006','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1006','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1006-Comm-Badge722")).
'find-or-create-constant'(string("ArtifactCol1006")).
:-osimAssert(isa('ArtifactCol1006-Comm-Badge722','ArtifactCol1006'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1006-Comm-Badge722',string("Comm Badge")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1006-Comm-Badge722',['The',['BodyPartCollectionFn','NpcCol1002-Worf720','Neck-AnimalBodyPart']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1003")).
:-osimAssert(isa('ArtifactCol1003','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1003','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1003-Gold-Uniform723")).
'find-or-create-constant'(string("ArtifactCol1003")).
:-osimAssert(isa('ArtifactCol1003-Gold-Uniform723','ArtifactCol1003'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1003-Gold-Uniform723',string("Gold Uniform")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1003-Gold-Uniform723',['The',['BodyPartCollectionFn','NpcCol1002-Worf720','Torso']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1000")).
:-osimAssert(isa('ArtifactCol1000','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1000','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1000-Phaser724")).
'find-or-create-constant'(string("ArtifactCol1000")).
:-osimAssert(isa('ArtifactCol1000-Phaser724','ArtifactCol1000'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1000-Phaser724',string("Phaser")),'*WorldCurrentStateMt*').
:-osimAssert(possesses('NpcCol1002-Worf720','ArtifactCol1000-Phaser724'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1007")).
:-osimAssert(isa('ArtifactCol1007','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1007','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1007-Sash725")).
'find-or-create-constant'(string("ArtifactCol1007")).
:-osimAssert(isa('ArtifactCol1007-Sash725','ArtifactCol1007'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1007-Sash725',string("Sash")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1007-Sash725',['The',['BodyPartCollectionFn','NpcCol1002-Worf720','Chest-BodyPart']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("NpcCol1010")).
:-osimAssert(isa('NpcCol1010','BPVAgentType'),'*WorldVocabularyMt*').
:-osimAssert(genls('NpcCol1010','Agent-Generic'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("NpcCol1010-Livingston726")).
'find-or-create-constant'(string("NpcCol1010")).
:-osimAssert(isa('NpcCol1010-Livingston726','NpcCol1010'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('NpcCol1010-Livingston726',string("Livingston")),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("Area1035")).
:-osimAssert(isa('Area1035','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert('in-ContCompletely'('NpcCol1010-Livingston726','Area1035'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("NpcCol1011")).
:-osimAssert(isa('NpcCol1011','BPVAgentType'),'*WorldVocabularyMt*').
:-osimAssert(genls('NpcCol1011','Agent-Generic'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("NpcCol1011-Spot727")).
'find-or-create-constant'(string("NpcCol1011")).
:-osimAssert(isa('NpcCol1011-Spot727','NpcCol1011'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('NpcCol1011-Spot727',string("Spot")),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("Area1003")).
:-osimAssert(isa('Area1003','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert('in-ContCompletely'('NpcCol1011-Spot727','Area1003'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("NpcCol1012")).
:-osimAssert(isa('NpcCol1012','BPVAgentType'),'*WorldVocabularyMt*').
:-osimAssert(genls('NpcCol1012','Agent-Generic'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("NpcCol1012-Ensign728")).
'find-or-create-constant'(string("NpcCol1012")).
:-osimAssert(isa('NpcCol1012-Ensign728','NpcCol1012'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('NpcCol1012-Ensign728',string("Ensign")),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("Area1000")).
:-osimAssert(isa('Area1000','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert('in-ContCompletely'('NpcCol1012-Ensign728','Area1000'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1005")).
:-osimAssert(isa('ArtifactCol1005','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1005','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1005-Boots729")).
'find-or-create-constant'(string("ArtifactCol1005")).
:-osimAssert(isa('ArtifactCol1005-Boots729','ArtifactCol1005'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1005-Boots729',string("Boots")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1005-Boots729',['The',['BodyPartCollectionFn','NpcCol1012-Ensign728','Foot-AnimalBodyPart']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1006")).
:-osimAssert(isa('ArtifactCol1006','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1006','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1006-Comm-Badge730")).
'find-or-create-constant'(string("ArtifactCol1006")).
:-osimAssert(isa('ArtifactCol1006-Comm-Badge730','ArtifactCol1006'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1006-Comm-Badge730',string("Comm Badge")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1006-Comm-Badge730',['The',['BodyPartCollectionFn','NpcCol1012-Ensign728','Neck-AnimalBodyPart']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1003")).
:-osimAssert(isa('ArtifactCol1003','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1003','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1003-Gold-Uniform731")).
'find-or-create-constant'(string("ArtifactCol1003")).
:-osimAssert(isa('ArtifactCol1003-Gold-Uniform731','ArtifactCol1003'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1003-Gold-Uniform731',string("Gold Uniform")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1003-Gold-Uniform731',['The',['BodyPartCollectionFn','NpcCol1012-Ensign728','Torso']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("NpcCol1012")).
:-osimAssert(isa('NpcCol1012','BPVAgentType'),'*WorldVocabularyMt*').
:-osimAssert(genls('NpcCol1012','Agent-Generic'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("NpcCol1012-Ensign732")).
'find-or-create-constant'(string("NpcCol1012")).
:-osimAssert(isa('NpcCol1012-Ensign732','NpcCol1012'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('NpcCol1012-Ensign732',string("Ensign")),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("Area1004")).
:-osimAssert(isa('Area1004','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert('in-ContCompletely'('NpcCol1012-Ensign732','Area1004'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1005")).
:-osimAssert(isa('ArtifactCol1005','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1005','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1005-Boots733")).
'find-or-create-constant'(string("ArtifactCol1005")).
:-osimAssert(isa('ArtifactCol1005-Boots733','ArtifactCol1005'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1005-Boots733',string("Boots")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1005-Boots733',['The',['BodyPartCollectionFn','NpcCol1012-Ensign732','Foot-AnimalBodyPart']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1006")).
:-osimAssert(isa('ArtifactCol1006','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1006','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1006-Comm-Badge734")).
'find-or-create-constant'(string("ArtifactCol1006")).
:-osimAssert(isa('ArtifactCol1006-Comm-Badge734','ArtifactCol1006'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1006-Comm-Badge734',string("Comm Badge")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1006-Comm-Badge734',['The',['BodyPartCollectionFn','NpcCol1012-Ensign732','Neck-AnimalBodyPart']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1003")).
:-osimAssert(isa('ArtifactCol1003','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1003','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1003-Gold-Uniform735")).
'find-or-create-constant'(string("ArtifactCol1003")).
:-osimAssert(isa('ArtifactCol1003-Gold-Uniform735','ArtifactCol1003'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1003-Gold-Uniform735',string("Gold Uniform")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1003-Gold-Uniform735',['The',['BodyPartCollectionFn','NpcCol1012-Ensign732','Torso']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("NpcCol1012")).
:-osimAssert(isa('NpcCol1012','BPVAgentType'),'*WorldVocabularyMt*').
:-osimAssert(genls('NpcCol1012','Agent-Generic'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("NpcCol1012-Ensign736")).
'find-or-create-constant'(string("NpcCol1012")).
:-osimAssert(isa('NpcCol1012-Ensign736','NpcCol1012'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('NpcCol1012-Ensign736',string("Ensign")),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("Area1011")).
:-osimAssert(isa('Area1011','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert('in-ContCompletely'('NpcCol1012-Ensign736','Area1011'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1005")).
:-osimAssert(isa('ArtifactCol1005','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1005','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1005-Boots737")).
'find-or-create-constant'(string("ArtifactCol1005")).
:-osimAssert(isa('ArtifactCol1005-Boots737','ArtifactCol1005'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1005-Boots737',string("Boots")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1005-Boots737',['The',['BodyPartCollectionFn','NpcCol1012-Ensign736','Foot-AnimalBodyPart']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1006")).
:-osimAssert(isa('ArtifactCol1006','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1006','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1006-Comm-Badge738")).
'find-or-create-constant'(string("ArtifactCol1006")).
:-osimAssert(isa('ArtifactCol1006-Comm-Badge738','ArtifactCol1006'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1006-Comm-Badge738',string("Comm Badge")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1006-Comm-Badge738',['The',['BodyPartCollectionFn','NpcCol1012-Ensign736','Neck-AnimalBodyPart']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1002")).
:-osimAssert(isa('ArtifactCol1002','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1002','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1002-Red-Uniform739")).
'find-or-create-constant'(string("ArtifactCol1002")).
:-osimAssert(isa('ArtifactCol1002-Red-Uniform739','ArtifactCol1002'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1002-Red-Uniform739',string("Red Uniform")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1002-Red-Uniform739',['The',['BodyPartCollectionFn','NpcCol1012-Ensign736','Torso']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("NpcCol1012")).
:-osimAssert(isa('NpcCol1012','BPVAgentType'),'*WorldVocabularyMt*').
:-osimAssert(genls('NpcCol1012','Agent-Generic'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("NpcCol1012-Ensign740")).
'find-or-create-constant'(string("NpcCol1012")).
:-osimAssert(isa('NpcCol1012-Ensign740','NpcCol1012'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('NpcCol1012-Ensign740',string("Ensign")),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("Area1020")).
:-osimAssert(isa('Area1020','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert('in-ContCompletely'('NpcCol1012-Ensign740','Area1020'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1005")).
:-osimAssert(isa('ArtifactCol1005','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1005','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1005-Boots741")).
'find-or-create-constant'(string("ArtifactCol1005")).
:-osimAssert(isa('ArtifactCol1005-Boots741','ArtifactCol1005'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1005-Boots741',string("Boots")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1005-Boots741',['The',['BodyPartCollectionFn','NpcCol1012-Ensign740','Foot-AnimalBodyPart']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1006")).
:-osimAssert(isa('ArtifactCol1006','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1006','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1006-Comm-Badge742")).
'find-or-create-constant'(string("ArtifactCol1006")).
:-osimAssert(isa('ArtifactCol1006-Comm-Badge742','ArtifactCol1006'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1006-Comm-Badge742',string("Comm Badge")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1006-Comm-Badge742',['The',['BodyPartCollectionFn','NpcCol1012-Ensign740','Neck-AnimalBodyPart']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1002")).
:-osimAssert(isa('ArtifactCol1002','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1002','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1002-Red-Uniform743")).
'find-or-create-constant'(string("ArtifactCol1002")).
:-osimAssert(isa('ArtifactCol1002-Red-Uniform743','ArtifactCol1002'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1002-Red-Uniform743',string("Red Uniform")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1002-Red-Uniform743',['The',['BodyPartCollectionFn','NpcCol1012-Ensign740','Torso']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("NpcCol1012")).
:-osimAssert(isa('NpcCol1012','BPVAgentType'),'*WorldVocabularyMt*').
:-osimAssert(genls('NpcCol1012','Agent-Generic'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("NpcCol1012-Ensign744")).
'find-or-create-constant'(string("NpcCol1012")).
:-osimAssert(isa('NpcCol1012-Ensign744','NpcCol1012'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('NpcCol1012-Ensign744',string("Ensign")),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("Area1024")).
:-osimAssert(isa('Area1024','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert('in-ContCompletely'('NpcCol1012-Ensign744','Area1024'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1005")).
:-osimAssert(isa('ArtifactCol1005','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1005','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1005-Boots745")).
'find-or-create-constant'(string("ArtifactCol1005")).
:-osimAssert(isa('ArtifactCol1005-Boots745','ArtifactCol1005'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1005-Boots745',string("Boots")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1005-Boots745',['The',['BodyPartCollectionFn','NpcCol1012-Ensign744','Foot-AnimalBodyPart']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1006")).
:-osimAssert(isa('ArtifactCol1006','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1006','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1006-Comm-Badge746")).
'find-or-create-constant'(string("ArtifactCol1006")).
:-osimAssert(isa('ArtifactCol1006-Comm-Badge746','ArtifactCol1006'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1006-Comm-Badge746',string("Comm Badge")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1006-Comm-Badge746',['The',['BodyPartCollectionFn','NpcCol1012-Ensign744','Neck-AnimalBodyPart']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1004")).
:-osimAssert(isa('ArtifactCol1004','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1004','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1004-Blue-Uniform747")).
'find-or-create-constant'(string("ArtifactCol1004")).
:-osimAssert(isa('ArtifactCol1004-Blue-Uniform747','ArtifactCol1004'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1004-Blue-Uniform747',string("Blue Uniform")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1004-Blue-Uniform747',['The',['BodyPartCollectionFn','NpcCol1012-Ensign744','Torso']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("NpcCol1012")).
:-osimAssert(isa('NpcCol1012','BPVAgentType'),'*WorldVocabularyMt*').
:-osimAssert(genls('NpcCol1012','Agent-Generic'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("NpcCol1012-Ensign748")).
'find-or-create-constant'(string("NpcCol1012")).
:-osimAssert(isa('NpcCol1012-Ensign748','NpcCol1012'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('NpcCol1012-Ensign748',string("Ensign")),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("Area1022")).
:-osimAssert(isa('Area1022','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert('in-ContCompletely'('NpcCol1012-Ensign748','Area1022'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1005")).
:-osimAssert(isa('ArtifactCol1005','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1005','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1005-Boots749")).
'find-or-create-constant'(string("ArtifactCol1005")).
:-osimAssert(isa('ArtifactCol1005-Boots749','ArtifactCol1005'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1005-Boots749',string("Boots")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1005-Boots749',['The',['BodyPartCollectionFn','NpcCol1012-Ensign748','Foot-AnimalBodyPart']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1006")).
:-osimAssert(isa('ArtifactCol1006','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1006','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1006-Comm-Badge750")).
'find-or-create-constant'(string("ArtifactCol1006")).
:-osimAssert(isa('ArtifactCol1006-Comm-Badge750','ArtifactCol1006'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1006-Comm-Badge750',string("Comm Badge")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1006-Comm-Badge750',['The',['BodyPartCollectionFn','NpcCol1012-Ensign748','Neck-AnimalBodyPart']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1004")).
:-osimAssert(isa('ArtifactCol1004','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1004','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1004-Blue-Uniform751")).
'find-or-create-constant'(string("ArtifactCol1004")).
:-osimAssert(isa('ArtifactCol1004-Blue-Uniform751','ArtifactCol1004'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1004-Blue-Uniform751',string("Blue Uniform")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1004-Blue-Uniform751',['The',['BodyPartCollectionFn','NpcCol1012-Ensign748','Torso']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("NpcCol1012")).
:-osimAssert(isa('NpcCol1012','BPVAgentType'),'*WorldVocabularyMt*').
:-osimAssert(genls('NpcCol1012','Agent-Generic'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("NpcCol1012-Ensign752")).
'find-or-create-constant'(string("NpcCol1012")).
:-osimAssert(isa('NpcCol1012-Ensign752','NpcCol1012'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('NpcCol1012-Ensign752',string("Ensign")),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("Area1036")).
:-osimAssert(isa('Area1036','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert('in-ContCompletely'('NpcCol1012-Ensign752','Area1036'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1005")).
:-osimAssert(isa('ArtifactCol1005','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1005','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1005-Boots753")).
'find-or-create-constant'(string("ArtifactCol1005")).
:-osimAssert(isa('ArtifactCol1005-Boots753','ArtifactCol1005'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1005-Boots753',string("Boots")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1005-Boots753',['The',['BodyPartCollectionFn','NpcCol1012-Ensign752','Foot-AnimalBodyPart']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1006")).
:-osimAssert(isa('ArtifactCol1006','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1006','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1006-Comm-Badge754")).
'find-or-create-constant'(string("ArtifactCol1006")).
:-osimAssert(isa('ArtifactCol1006-Comm-Badge754','ArtifactCol1006'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1006-Comm-Badge754',string("Comm Badge")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1006-Comm-Badge754',['The',['BodyPartCollectionFn','NpcCol1012-Ensign752','Neck-AnimalBodyPart']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1004")).
:-osimAssert(isa('ArtifactCol1004','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1004','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1004-Blue-Uniform755")).
'find-or-create-constant'(string("ArtifactCol1004")).
:-osimAssert(isa('ArtifactCol1004-Blue-Uniform755','ArtifactCol1004'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1004-Blue-Uniform755',string("Blue Uniform")),'*WorldCurrentStateMt*').
:-osimAssert(wornOn('ArtifactCol1004-Blue-Uniform755',['The',['BodyPartCollectionFn','NpcCol1012-Ensign752','Torso']]),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1010")).
:-osimAssert(isa('ArtifactCol1010','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1010','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1010-Dilithium-Crystal756")).
'find-or-create-constant'(string("ArtifactCol1010")).
:-osimAssert(isa('ArtifactCol1010-Dilithium-Crystal756','ArtifactCol1010'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1010-Dilithium-Crystal756',string("Dilithium Crystal")),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("Area1000")).
:-osimAssert(isa('Area1000','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert('in-ContCompletely'('ArtifactCol1010-Dilithium-Crystal756','Area1000'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1010")).
:-osimAssert(isa('ArtifactCol1010','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1010','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1010-Dilithium-Crystal757")).
'find-or-create-constant'(string("ArtifactCol1010")).
:-osimAssert(isa('ArtifactCol1010-Dilithium-Crystal757','ArtifactCol1010'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1010-Dilithium-Crystal757',string("Dilithium Crystal")),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("Area1000")).
:-osimAssert(isa('Area1000','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert('in-ContCompletely'('ArtifactCol1010-Dilithium-Crystal757','Area1000'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1010")).
:-osimAssert(isa('ArtifactCol1010','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1010','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1010-Dilithium-Crystal758")).
'find-or-create-constant'(string("ArtifactCol1010")).
:-osimAssert(isa('ArtifactCol1010-Dilithium-Crystal758','ArtifactCol1010'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1010-Dilithium-Crystal758',string("Dilithium Crystal")),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("Area1000")).
:-osimAssert(isa('Area1000','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert('in-ContCompletely'('ArtifactCol1010-Dilithium-Crystal758','Area1000'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1009")).
:-osimAssert(isa('ArtifactCol1009','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1009','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1009-Tricorder759")).
'find-or-create-constant'(string("ArtifactCol1009")).
:-osimAssert(isa('ArtifactCol1009-Tricorder759','ArtifactCol1009'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1009-Tricorder759',string("Tricorder")),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("Area1015")).
:-osimAssert(isa('Area1015','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert('in-ContCompletely'('ArtifactCol1009-Tricorder759','Area1015'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1009")).
:-osimAssert(isa('ArtifactCol1009','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1009','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1009-Tricorder760")).
'find-or-create-constant'(string("ArtifactCol1009")).
:-osimAssert(isa('ArtifactCol1009-Tricorder760','ArtifactCol1009'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1009-Tricorder760',string("Tricorder")),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("Area1015")).
:-osimAssert(isa('Area1015','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert('in-ContCompletely'('ArtifactCol1009-Tricorder760','Area1015'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("ArtifactCol1009")).
:-osimAssert(isa('ArtifactCol1009','ArtifactTypeByGenericCategory'),'*WorldVocabularyMt*').
:-osimAssert(isa('ArtifactCol1009','BPVArtifactType'),'*WorldVocabularyMt*').
'find-or-create-constant'(string("ArtifactCol1009-Tricorder761")).
'find-or-create-constant'(string("ArtifactCol1009")).
:-osimAssert(isa('ArtifactCol1009-Tricorder761','ArtifactCol1009'),'*WorldCurrentStateMt*').
:-osimAssert(nameString('ArtifactCol1009-Tricorder761',string("Tricorder")),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("Area1015")).
:-osimAssert(isa('Area1015','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert('in-ContCompletely'('ArtifactCol1009-Tricorder761','Area1015'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("Area1022")).
:-osimAssert(isa('Area1022','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1022','South-Directly'],'ClosedPortal'),'*WorldCurrentStateMt*').
'find-or-create-constant'(string("Area1039")).
:-osimAssert(isa('Area1039','BPVLocation'),'*WorldStaticStateMt*').
:-osimAssert(isa(['BoundsOfDirectionFn','Area1039','North-Directly'],'ClosedPortal'),'*WorldCurrentStateMt*').
%% pipequoted("load file '10.zon.kif'").
%% pipequoted("load file 'CoffeeMud_4_7_4[1].zip'").
%% pipequoted("load file 'map.txt'").
%% pipequoted("load file 'MICRO-IRAQ-ART-CONVT.zip'").
%% pipequoted("load file 'readme.txt'").
%% pipequoted("load file 'worlds.rar'").
%% pipequoted("load file 'zone.lst'").
:-osimAssert(back_quoted('locatedAtPoint-Spatial'('Area1000',['Point3Fn',100,100,0])),'*StaticStateMt*').

'fi-kill'('find-or-create-constant'(string("Area-1"))).
'fi-kill'('find-or-create-constant'(string("Area4075"))).

%% pipequoted("load file 'zone.lst'").
:-osimAssert(osimItemToName(['OSimItemFn',string("cyc_bot_1")],string("cyc_bot_1")),'OSimInitialStateMt').
:-osimAssert(osimItemToName(['OSimItemFn',string("player1")],string("player1")),'OSimInitialStateMt').

:-osimAssert(osimGenls('ArtifactCol1010',['OSimFacetFn',string("classname"),string("dilithium_crystal")]),'*MappingMt*').
:-osimAssert(osimGenls('ArtifactCol1024',['OSimFacetFn',string("classname"),string("small_glass")]),'*MappingMt*').
:-osimAssert(osimGenls('ArtifactCol1023',['OSimFacetFn',string("classname"),string("romulan_bottle")]),'*MappingMt*').
:-osimAssert(osimGenls('ArtifactCol1022',['OSimFacetFn',string("classname"),string("ferengi_bottle")]),'*MappingMt*').
:-osimAssert(osimGenls('ArtifactCol1021',['OSimFacetFn',string("classname"),string("moveable_plasticjar1")]),'*MappingMt*').
:-osimAssert(osimGenls('ArtifactCol1020',['OSimFacetFn',string("classname"),string("moveable_foamcup")]),'*MappingMt*').
:-osimAssert(osimGenls('ArtifactCol1006',['OSimFacetFn',string("classname"),string("starfleet_communication_badge")]),'*MappingMt*').
:-osimAssert(osimIsa(['OSimItemFn',string("player1")],['OSimFacetFn',string("rezclass"),string("idPlayer")]),'OSimInitialStateMt').
:-osimAssert(osimIsa(['OSimItemFn',string("cyc_bot_1")],['OSimFacetFn',string("rezclass"),string("idAI")]),'OSimInitialStateMt').


