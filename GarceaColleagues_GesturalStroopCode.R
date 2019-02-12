#####Model 1
#####Test for Order Effect
Model1_FullModel <- glmer(HandPostureAcc ~ Order + Congruence * Group + (1| Subject) + (1| Item), family = binomial, data = Model1)
Model1_OrderTest <- glmer(HandPostureAcc ~ Congruence * Group + (1| Subject) + (1| Item), family = binomial, data = Model1)

anova(Model1_FullModel, Model1_OrderTest)

#Since test for order was not significant new full model
Model1_FullModel <- glmer(HandPostureAcc ~ Congruence * Group + (1| Subject) + (1| Item), family = binomial, data = Model1)

#####Test of interaction
Model1_NoInteraction <- glmer(HandPostureAcc ~ Congruence + Group + (1| Subject) + (1| Item), family = binomial, data = Model1)

anova(Model1_FullModel, Model1_NoInteraction)

#####Test of Congruence
Model1_GestType <- glmer(HandPostureAcc ~ Group + (1| Subject) + (1| Item), family = binomial, data = Model1)

anova(Model1_NoInteraction, Model1_GestType)

#####Model 2
#####Test for Order Effect
Model2_FullModel <- glmer(HandPostureAcc ~ Order + NeighborType * Group + (1| Subject) + (1| Item), family = binomial, data = Model2)
Model2_OrderTest <- glmer(HandPostureAcc ~ NeighborType * Group + (1| Subject) + (1| Item), family = binomial, data = Model2)

anova(Model2_FullModel, Model2_OrderTest)

#Since test for order was not significant new full model
Model2_FullModel <- glmer(HandPostureAcc ~ NeighborType * Group + (1| Subject) + (1| Item), family = binomial, data = Model2)

#####Test of interaction
Model2_NoInteraction <- glmer(HandPostureAcc ~ NeighborType + Group + (1| Subject) + (1| Item), family = binomial, data = Model2)

anova(Model2_FullModel, Model2_NoInteraction)

#Further probing of interaction 
Model2_Interaction_Action <- glmer(HandPostureAcc ~ Group + (1| Subject) + (1| Item), family = binomial, data = Model2)
Model2_Interaction_Action2 <- glmer(HandPostureAcc ~ (1| Subject) + (1| Item), family = binomial, data = Model2)
Model2_Interaction_Function <- glmer(HandPostureAcc ~ Group + (1| Subject) + (1| Item), family = binomial, data = Model2)
Model2_Interaction_Function2 <- glmer(HandPostureAcc ~ (1| Subject) + (1| Item), family = binomial, data = Model2)
Model2_Interaction_Unrelated <- glmer(HandPostureAcc ~ Group + (1| Subject) + (1| Item), family = binomial, data = Model2)
Model2_Interaction_Unrelated2 <- glmer(HandPostureAcc ~ (1| Subject) + (1| Item), family = binomial, data = Model2)

anova(Model2_Interaction_Action, Model2_Interaction_Action2)
anova(Model2_Interaction_Function, Model2_Interaction_Function2)
anova(Model2_Interaction_Unrelated, Model2_Interaction_Unrelated2)

#####Test of Tool Neighbor
Model2_NeighType <- glmer(HandPostureAcc ~ Group + (1| Subject) + (1| Item), family = binomial, data = Model2)

anova(Model2_NoInteraction, Model2_NeighType)

#Further probing of Tool Neighbor effect
Model2_NeighType_ActFun <- glmer(HandPostureAcc ~ NeighborType + Group + (1| Subject) + (1| Item), family = binomial, data = Model2)
Model2_NeighType_ActFun2 <- glmer(HandPostureAcc ~ Group + (1| Subject) + (1| Item), family = binomial, data = Model2)
Model2_NeighType_UnreFun <- glmer(HandPostureAcc ~ NeighborType + Group + (1| Subject) + (1| Item), family = binomial, data = Model2)
Model2_NeighType_UnreFun2 <- glmer(HandPostureAcc ~ Group + (1| Subject) + (1| Item), family = binomial, data = Model2)
Model2_NeighType_ActUnre <- glmer(HandPostureAcc ~ NeighborType + Group + (1| Subject) + (1| Item), family = binomial, data = Model2)
Model2_NeighType_ActUnre2 <- glmer(HandPostureAcc ~ Group + (1| Subject) + (1| Item), family = binomial, data = Model2)

anova(Model2_NeighType_ActFun, Model2_NeighType_ActFun2)
anova(Model2_NeighType_UnreFun, Model2_NeighType_UnreFun2)
anova(Model2_NeighType_ActUnre, Model2_NeighType_ActUnre2)


#####Model 3
#####Testing for effect of Order
Model3_FullModel <- glmer(HandPostureAcc ~ Order + NeighborType * Congruence + (1| Subject) + (1| Item), family = binomial, data = Model3)
Model3_OrderTest <- glmer(HandPostureAcc ~ NeighborType * Congruence + (1| Subject) + (1| Item), family = binomial, data = Model3)

anova(Model3_FullModel, Model3_OrderTest)

#####Testing of interaction
Model3_NoInteraction <- glmer(HandPostureAcc ~ Order + NeighborType + Congruence + (1| Subject) + (1| Item), family = binomial, data = Model3)

anova(Model3_FullModel, Model3_NoInteraction)

#Further probing of interaction
Model3_Interaction_ActFun <- glmer(HandPostureAcc ~ Order + Congruence * NeighborType + (1| Subject) + (1| Item), family = binomial, data = Model3)
Model3_Interaction_ActFun2 <- glmer(HandPostureAcc ~ Order + Congruence + NeighborType + (1| Subject) + (1| Item), family = binomial, data = Model3)
Model3_Interaction_ActUnre <- glmer(HandPostureAcc ~ Order + Congruence * NeighborType + (1| Subject) + (1| Item), family = binomial, data = Model3)
Model3_Interaction_ActUnre2 <- glmer(HandPostureAcc ~ Order + Congruence + NeighborType + (1| Subject) + (1| Item), family = binomial, data = Model3)
Model3_Interaction_UnreFun <- glmer(HandPostureAcc ~ Order + Congruence * NeighborType + (1| Subject) + (1| Item), family = binomial, data = Model3)
Model3_Interaction_UnreFun2 <- glmer(HandPostureAcc ~ Order + Congruence + NeighborType + (1| Subject) + (1| Item), family = binomial, data = Model3)

anova(Model3_Interaction_ActFun,Model3_Interaction_ActFun2)
anova(Model3_Interaction_ActUnre,Model3_Interaction_ActUnre2)
anova(Model3_Interaction_UnreFun,Model3_Interaction_UnreFun2)

#####Testing of Main Effects (Tool Neighbor and Congruence)
Model3_ToolNeighbor <- glmer(HandPostureAcc ~ Order + Congruence + (1| Subject) + (1| Item), family = binomial, data = Model3)
Model3_Congruence <- glmer(HandPostureAcc ~ Order + NeighborType + (1| Subject) + (1| Item), family = binomial, data = Model3)

anova(Model3_NoInteraction, Model3_ToolNeighbor)
anova(Model3_NoInteraction, Model3_Congruence)

#Further probing of Tool Neighbor effect
Model3_ToolNeighbor_ActFun <- glmer(HandPostureAcc ~ Order + NeighborType + Congruence + (1| Subject) + (1| Item), family = binomial, data = Model3)
Model3_ToolNeighbor_ActFun2 <- glmer(HandPostureAcc ~ Order + Congruence + (1| Subject) + (1| Item), family = binomial, data = Model3)
Model3_ToolNeighbor_UnreFun <- glmer(HandPostureAcc ~ Order + NeighborType + Congruence + (1| Subject) + (1| Item), family = binomial, data = Model3)
Model3_ToolNeighbor_UnreFun2 <- glmer(HandPostureAcc ~ Order + Congruence + (1| Subject) + (1| Item), family = binomial, data = Model3)
Model3_ToolNeighbor_ActUnre <- glmer(HandPostureAcc ~ Order + NeighborType + Congruence + (1| Subject) + (1| Item), family = binomial, data = Model3)
Model3_ToolNeighbor_ActUnre2 <- glmer(HandPostureAcc ~ Order + Congruence + (1| Subject) + (1| Item), family = binomial, data = Model3)

anova(Model3_ToolNeighbor_ActFun, Model3_ToolNeighbor_ActFun2)
anova(Model3_ToolNeighbor_UnreFun, Model3_ToolNeighbor_UnreFun2)
anova(Model3_ToolNeighbor_ActUnre, Model3_ToolNeighbor_ActUnre2)

######Model 4
#####Test of Order Effect
Model4_FullModel <- glmer(HandPostureAcc ~ Order + CongruentAcc + NeighborType * GraspUseScore + (1| Subject) + (1| Item), family = binomial, data = Model4)
Model4_OrderTest <- glmer(HandPostureAcc ~ CongruentAcc + NeighborType * GraspUseScore + (1| Subject) + (1| Item), family = binomial, data = Model4)

anova(Model4_FullModel, Model4_OrderTest)

#Since test for order was not significant new full model
Model4_FullModel <- glmer(HandPostureAcc ~ CongruentAcc + NeighborType * GraspUseScore + (1| Subject) + (1| Item), family = binomial, data = Model4)

#####Test of Interaction
Model4_NoInteraction <- glmer(HandPostureAcc ~ CongruentAcc + NeighborType + GraspUseScore + (1| Subject) + (1| Item), family = binomial, data = Model4)

anova(Model4_FullModel, Model4_NoInteraction)

#####Breakdown of Interaction
Model4_Interaction_ActFun <- glmer(HandPostureAcc ~ CongruentAcc + NeighborType * GraspUseScore + (1| Subject) + (1| Item), family = binomial, data = Model4)
Model4_Interaction_ActFun2 <- glmer(HandPostureAcc ~ CongruentAcc + NeighborType + GraspUseScore + (1| Subject) + (1| Item), family = binomial, data = Model4)
Model4_Interaction_UnreFun <- glmer(HandPostureAcc ~ CongruentAcc + NeighborType * GraspUseScore + (1| Subject) + (1| Item), family = binomial, data = Model4)
Model4_Interaction_UnreFun2 <- glmer(HandPostureAcc ~ CongruentAcc + NeighborType + GraspUseScore + (1| Subject) + (1| Item), family = binomial, data = Model4)
Model4_Interaction_ActUnre <- glmer(HandPostureAcc ~ CongruentAcc + NeighborType * GraspUseScore + (1| Subject) + (1| Item), family = binomial, data = Model4)
Model4_Interaction_ActUnre2 <- glmer(HandPostureAcc ~ CongruentAcc + NeighborType + GraspUseScore + (1| Subject) + (1| Item), family = binomial, data = Model4)

anova(Model4_Interaction_ActFun, Model4_Interaction_ActFun2)
anova(Model4_Interaction_UnreFun, Model4_Interaction_UnreFun2)
anova(Model4_Interaction_ActUnre, Model4_Interaction_ActUnre2)