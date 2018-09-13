;;; Generated from pvs-set-prelude-info.el - do not edit
(defvar *prelude-files-and-regions*
  (mapcar
      '(lambda (x)
	 (cons (format "%s/lib/%s" pvs-path (car x)) (cdr x)))
    '(("character_adt.pvs" ("character_adt" 1 2491) ("character_adt_reduce" 2491 3182)) ("lift_adt.pvs" ("lift_adt" 1 3063) ("lift_adt_map" 3063 3541) ("lift_adt_reduce" 3541 4285)) ("list_adt.pvs" ("list_adt" 1 4258) ("list_adt_map" 4258 4919) ("list_adt_reduce" 4919 5806)) ("ordstruct_adt.pvs" ("ordstruct_adt" 1 3733) ("ordstruct_adt_reduce" 3733 4814)) ("prelude.pvs" ("booleans" 1 2428) ("equalities" 2428 2641) ("notequal" 2641 2739) ("if_def" 2739 3019) ("boolean_props" 3019 4074) ("xor_def" 4074 4378) ("quantifier_props" 4378 5375) ("defined_types" 5375 5677) ("exists1" 5677 6294) ("equality_props" 6294 6765) ("if_props" 6765 7136) ("functions" 7136 8684) ("functions_alt" 8684 8913) ("transpose" 8913 9061) ("restrict" 9061 9750) ("restrict_props" 9750 10151) ("extend" 10151 10547) ("extend_bool" 10547 10810) ("extend_props" 10810 11210) ("extend_func_props" 11210 11800) ("K_conversion" 11800 12034) ("K_props" 12034 12242) ("identity" 12242 12600) ("identity_props" 12600 12837) ("relations" 12837 14003) ("orders" 14003 17088) ("orders_alt" 17088 17627) ("restrict_order_props" 17627 19672) ("extend_order_props" 19672 20434) ("wf_induction" 20434 20778) ("measure_induction" 20778 21215) ("epsilons" 21215 21870) ("sets" 21870 24949) ("sets_lemmas" 24949 33962) ("function_inverse_def" 33962 38663) ("function_inverse" 38663 40813) ("function_inverse_alt" 40813 42157) ("function_image" 42157 43755) ("function_props" 43755 44974) ("function_props_alt" 44974 45587) ("function_props2" 45587 45930) ("relation_defs" 45930 47417) ("relation_props" 47417 48002) ("relation_props2" 48002 48202) ("relation_converse_props" 48202 50002) ("indexed_sets" 50002 50874) ("operator_defs" 50874 51719) ("numbers" 51719 52049) ("number_fields" 52049 53824) ("reals" 53824 55543) ("real_axioms" 55543 56360) ("bounded_real_defs" 56360 57946) ("bounded_real_defs_alt" 57946 58454) ("real_types" 58454 60675) ("rationals" 60675 64117) ("integers" 64117 68500) ("naturalnumbers" 68500 70712) ("min_nat" 70712 71017) ("real_defs" 71017 75192) ("real_props" 75192 88459) ("extra_real_props" 88459 97879) ("extra_tegies" 97879 98414) ("rational_props" 98414 98970) ("integer_props" 98970 99659) ("floor_ceil" 99659 101958) ("exponentiation" 101958 108635) ("euclidean_division" 108635 109677) ("divides" 109677 112169) ("modulo_arithmetic" 112169 115674) ("subrange_inductions" 115674 116319) ("bounded_int_inductions" 116319 117103) ("bounded_nat_inductions" 117103 117926) ("subrange_type" 117926 118039) ("int_types" 118039 118249) ("nat_types" 118249 118455) ("nat_fun_props" 118455 120156) ("finite_sets" 120156 128456) ("restrict_set_props" 128456 128949) ("extend_set_props" 128949 131005) ("function_image_aux" 131005 131911) ("function_iterate" 131911 132738) ("sequences" 132738 134197) ("seq_functions" 134197 134519) ("finite_sequences" 134519 136379) ("more_finseq" 136379 136841) ("ordstruct" 136841 137049) ("ordinals" 137049 138921) ("lex2" 138921 139429) ("list" 139429 139573) ("list_props" 139573 141671) ("map_props" 141671 142055) ("more_map_props" 142055 142305) ("filters" 142305 143202) ("list2finseq" 143202 143752) ("list2set" 143752 144009) ("disjointness" 144009 144611) ("character" 144611 145936) ("strings" 145936 147713) ("lift" 147713 147793) ("union" 147793 147888) ("mucalculus" 147888 149148) ("ctlops" 149148 149883) ("fairctlops" 149883 151178) ("Fairctlops" 151178 152586) ("bit" 152586 153232) ("bv" 153232 154042) ("exp2" 154042 154784) ("bv_concat_def" 154784 155437) ("bv_bitwise" 155437 156563) ("bv_nat" 156563 159694) ("empty_bv" 159694 159799) ("bv_caret" 159799 161596) ("mod" 161596 165441) ("bv_arith_nat_defs" 165441 168068) ("bv_int_defs" 168068 169398) ("bv_arithmetic_defs" 169398 171286) ("bv_extend_defs" 171286 173700) ("infinite_sets_def" 173700 175730) ("finite_sets_of_sets" 175730 178246) ("EquivalenceClosure" 178246 179185) ("QuotientDefinition" 179185 180483) ("KernelDefinition" 180483 181117) ("QuotientKernelProperties" 181117 181902) ("QuotientSubDefinition" 181902 182221) ("QuotientExtensionProperties" 182221 182898) ("QuotientDistributive" 182898 185395) ("QuotientIteration" 185395 186176) ("PartialFunctionDefinitions" 186176 187705) ("PartialFunctionComposition" 187705 188605) ("stdlang" 188605 189098) ("stdexc" 189098 189400) ("stdcatch" 189400 190379) ("stdprog" 190379 191718) ("stdglobal" 191718 191874) ("stdpvs" 191874 192196) ("stdstr" 192196 195139) ("stdio" 195139 202468) ("stdmath" 202468 204113) ("stdfmap" 204113 205203) ("stdindent" 205203 206473) ("stdtokenizer" 206473 214866) ("stdpvsio" 214866 215089) ("stdsys" 215089 216993)) ("union_adt.pvs" ("union_adt" 1 3763) ("union_adt_map" 3763 4530) ("union_adt_reduce" 4530 5382)))))