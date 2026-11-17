%% ==================================================================
% REGRESSION ANALYSIS: Female Labour Force Participation
% Table 1: Changes in LFPR across survey rounds
% Table 2: Regression - overall cohort comparison (cohort-level data)
% Table 3: Regression - rural women (cohort-level data)
% Table 4: Regression - urban women (cohort-level data)
% Table 5: Regression - all women (single-year-of-age data)
%
% LFPR = a + b1*Age + b2*Younger + b3*(Age x Younger) + e
% ===================================================================

clear; 
clc; 
close all;

filename = 'lfpr_cohort_master.xlsx';
opts = detectImportOptions(filename, 'Sheet', 'Sheet1');
opts = setvartype(opts, {'cohort','group','measure','round'}, 'string');
T = readtable(filename, opts);

% ---------- Round order, labels, survey years (shared) ----------
round_order = ["nss55";"nss61";"nss62";"nss64";"nss66";"nss68"; ...
               "plfs1718";"plfs1819";"plfs1920";"plfs2021"; ...
               "plfs2122";"plfs2223";"plfs2324"];

round_labels = ["NSS 55th (1999-00)";"NSS 61st (2004-05)";"NSS 62nd (2005-06)"; ...
                "NSS 64th (2007-08)";"NSS 66th (2009-10)";"NSS 68th (2011-12)"; ...
                "PLFS 2017-18";"PLFS 2018-19";"PLFS 2019-20";"PLFS 2020-21"; ...
                "PLFS 2021-22";"PLFS 2022-23";"PLFS 2023-24"];

% Mid-point of each survey year (July-June reference period)
round_year = [1999.5; 2004.5; 2005.5; 2007.5; 2009.5; 2011.5; ...
              2017.5; 2018.5; 2019.5; 2020.5; 2021.5; 2022.5; 2023.5];

%% ===================================================================
% Changes by round Table and Regressions Tables
% ====================================================================

D = T(T.measure == "UPSS, all", :);
D = D(ismember(D.cohort, ["1980-84","1985-89"]), :);
D = D(ismember(D.group,  ["All","Rural","Urban"]), :);
D = D(~ismissing(D.est_pct), :);

[found, D.RoundOrder] = ismember(D.round, round_order);
if any(~found)
    error('Some survey rounds are missing from round_order.');
end

D.SurveyRound = round_labels(D.RoundOrder);
D.SurveyYear  = round_year(D.RoundOrder);

% Cohort mid-point birth year and age at survey
D.BirthYear = nan(height(D),1);
D.BirthYear(D.cohort == "1980-84") = 1982;
D.BirthYear(D.cohort == "1985-89") = 1987;
D.Age = D.SurveyYear - D.BirthYear;

% Younger cohort dummy (1 = 1985-89, 0 = 1980-84)
D.Younger = double(D.cohort == "1985-89");

% Centre age on the sample mean (about 30.14)
D.Age_c = D.Age - mean(D.Age);

% Interaction
D.Age_Younger = D.Age_c .* D.Younger;

D = sortrows(D, {'group','cohort','RoundOrder'});

%% ==========================================
% Table 1: Changes in LFPR across rounds
% ===========================================

D.Change_pp     = nan(height(D),1);
D.PreviousRound = strings(height(D),1);

keys = unique(D(:, {'cohort','group'}));
for i = 1:height(keys)
    rows = find(D.cohort == keys.cohort(i) & D.group == keys.group(i));
    [~, ord] = sort(D.RoundOrder(rows));
    rows = rows(ord);
    for j = 2:numel(rows)
        D.Change_pp(rows(j))     = D.est_pct(rows(j)) - D.est_pct(rows(j-1));
        D.PreviousRound(rows(j)) = D.SurveyRound(rows(j-1));
    end
end

Table1 = table(D.SurveyRound, D.cohort, D.group, D.est_pct, D.se_pct, ...
               D.lo_pct, D.hi_pct, D.PreviousRound, D.Change_pp, ...
    'VariableNames', {'SurveyRound','Cohort','Residence','LFPR_pct', ...
    'SE_pct','CI_Lower','CI_Upper','PreviousRound','Change_pp'});

Table1.Order = D.RoundOrder;
Table1 = sortrows(Table1, {'Residence','Cohort','Order'});
Table1.Order = [];

disp('TABLE 1: CHANGES IN FEMALE LFPR ACROSS SURVEY ROUNDS');
disp(Table1);
writetable(Table1, 'Table1_LFPR_Changes.xlsx', 'Sheet', 'Table 1');

%% ===========================================
% Tables 2-4: Regressions on Cohort Data
% ============================================

spec = 'est_pct ~ Age_c + Younger + Age_Younger';

model_all   = fitlm(D(D.group=="All",   :), spec);
model_rural = fitlm(D(D.group=="Rural", :), spec);
model_urban = fitlm(D(D.group=="Urban", :), spec);

disp('TABLE 2: OVERALL COHORT COMPARISON');  
disp(model_all);
disp('TABLE 3: RURAL WOMEN');                
disp(model_rural);
disp('TABLE 4: URBAN WOMEN');                
disp(model_urban);

% Export Tables 2-4
models = {model_all, model_rural, model_urban};
sheets = {'Overall','Rural','Urban'};
for k = 1:3
    C = models{k}.Coefficients;
    C = addvars(C, string(C.Properties.RowNames), 'Before', 1, ...
        'NewVariableNames', 'Term');
    C.Properties.RowNames = {};
    writetable(C, 'Regression_Results.xlsx', 'Sheet', sheets{k});
end

%% ====================================================================
% Table 5: All Women, by Single-Year-of-Age Data
% Coefficient observed: cohort x survey round x single year of age
% =====================================================================

S = T(T.measure == "UPSS by single year of age", :);
S = S(ismember(S.cohort, ["1980-84","1985-89"]), :);
S = S(~ismissing(S.est_pct) & ~ismissing(S.se_pct), :);

% Age taken from the .xlsx file
S.Age = str2double(S.group);
if any(isnan(S.Age))
    error('Some values in "group" are not numeric ages.');
end

[found5, S.RoundID] = ismember(S.round, round_order);
if any(~found5)
    error('Some survey rounds in the single-year data are missing from round_order.');
end
S.CellID = findgroups(S.round, S.cohort); 

S.Younger     = double(S.cohort == "1985-89");
S.Age_c       = S.Age - mean(S.Age);        % mean age = 30.14
S.Age_Younger = S.Age_c .* S.Younger;
S.w           = 1 ./ (S.se_pct .^ 2);       % inverse-variance weights

N5 = height(S);
y5 = S.est_pct;

% Baseline: classical pooled OLS
model_t5 = fitlm(S, 'est_pct ~ Age_c + Younger + Age_Younger');
disp('TABLE 5: ALL WOMEN, SINGLE-YEAR-OF-AGE DATA');
disp('Pooled OLS, classical standard errors');
disp(model_t5);

% Design matrices: pooled + round fixed effects
X0 = [ones(N5,1), S.Age_c, S.Younger, S.Age_Younger];
FE = dummyvar(S.RoundID);
X1 = [X0, FE(:, 2:end)];

% Specifications with cluster-robust standard errors
specs5 = {'Pooled OLS, cluster by cohort-round', X0, ones(N5,1), S.CellID
          'Pooled OLS, cluster by round',        X0, ones(N5,1), S.RoundID
          'Round FE, OLS, cluster by round',     X1, ones(N5,1), S.RoundID
          'Round FE, WLS, cluster by round',     X1, S.w,        S.RoundID
         };

term_names = ["Intercept";"Age_c";"Younger";"Age_Younger"];
Table5  = table();
Table5c = table();

for k = 1:size(specs5, 1)
    r = cluster_reg(y5, specs5{k,2}, specs5{k,3}, specs5{k,4}, [3 4]);

    fprintf('\n%s  (clusters = %d)\n', specs5{k,1}, r.G);
    coef_k = table(term_names, r.b(1:4), r.se(1:4), r.t(1:4), r.p(1:4), ...
        'VariableNames', {'Term','Coef','SE','t','pValue'});
    disp(coef_k);
    fprintf('Joint test Younger = Age_Younger = 0:  F = %.3f, p = %.4f\n', ...
        r.F_joint, r.p_joint);

    row = table(string(specs5{k,1}), r.b(3), r.se(3), r.p(3), ...
        r.b(4), r.se(4), r.p(4), r.p_joint, ...
        'VariableNames', {'Specification','Younger','Younger_SE','Younger_p', ...
        'AgeXYounger','AgeXYounger_SE','AgeXYounger_p','Joint_p'});
    Table5 = [Table5; row]; %#ok<AGROW>

    coef_k = addvars(coef_k, repmat(string(specs5{k,1}), 4, 1), ...
        'Before', 1, 'NewVariableNames', 'Specification');
    Table5c = [Table5c; coef_k]; %#ok<AGROW>
end

disp(' ');
disp('TABLE 5 SUMMARY: cohort differences across specifications');
disp(Table5);

% Exports Table 5 into the same workbook as Tables 2-4
writetable(Table5,  'Regression_Results.xlsx', 'Sheet', 'Table5_Summary');
writetable(Table5c, 'Regression_Results.xlsx', 'Sheet', 'Table5_Coefficients');
disp('All tables exported successfully.');


%% ============================================================================
% Table 6: All Women, Quadratic Age + Round FIXED EFFECTS
% LFPR = a + b1*Age + b2*Age^2 + b3*Younger + b4*(Age x Younger)
%        + b5*(Age^2 x Younger) + round FE + e
% Age is centred at its mean (30.14) -> b3 is the cohort gap at the mean age
% Standard errors are clustered by survey round (13 clusters); p-values use
% t(G-1). Since 13 clusters is too little, the joint cohort test is reported
% with a wild cluster bootstrap p-value.
% =============================================================================

S.Age_c2        = S.Age_c .^ 2;
S.Age2_Younger  = S.Age_c2 .* S.Younger;
 
X6 = [ones(N5,1), S.Age_c, S.Age_c2, S.Younger, S.Age_Younger, ...
      S.Age2_Younger, FE(:, 2:end)];
cohort_idx    = [4 5 6];   % Younger, Age_Younger, Age2_Younger
curvature_idx = [3 6];     % Age_c2, Age2_Younger
 
r6 = cluster_reg(y5, X6, ones(N5,1), S.RoundID, cohort_idx);
 
term6 = ["Intercept";"Age_c";"Age_c2";"Younger";"Age_Younger";"Age2_Younger"];
Table6 = table(term6, r6.b(1:6), r6.se(1:6), r6.t(1:6), r6.p(1:6), ...
              'VariableNames', {'Term','Coef','SE','t','pValue'});
 
disp(' ');
disp('TABLE 6: ALL WOMEN, QUADRATIC AGE + ROUND FIXED EFFECTS');
fprintf('(single-year-of-age data, N = %d, clusters = %d)\n', N5, r6.G);
disp(Table6);
 
% --- Joint tests ---
r6_curv = cluster_reg(y5, X6, ones(N5,1), S.RoundID, curvature_idx);
[~, p_boot_quad] = wild_cluster_boot(y5, X6, S.RoundID, cohort_idx);
 
% Comparison: linear model with round FE (X1 from Table 5)
r_lin = cluster_reg(y5, X1, ones(N5,1), S.RoundID, [3 4]);
[~, p_boot_lin] = wild_cluster_boot(y5, X1, S.RoundID, [3 4]);
 
% Classical nested F-test: linear vs quadratic (both with round FE)
rss  = @(X) sum((y5 - X * (X \ y5)).^2);
rss1 = rss(X1);   rss2 = rss(X6);
df2  = N5 - size(X6, 2);
F_nest = ((rss1 - rss2) / 2) / (rss2 / df2);
p_nest = 1 - fcdf(F_nest, 2, df2);
 
Table6_tests = table( ...
    ["Quadratic: cohort terms (Younger, Age_Younger, Age2_Younger) = 0"; ...
     "Quadratic: curvature terms (Age_c2, Age2_Younger) = 0"; ...
     "Linear (Table 5 round FE): cohort terms (Younger, Age_Younger) = 0"; ...
     "Nested F-test: linear vs quadratic (classical)"], ...
    [r6.F_joint; r6_curv.F_joint; r_lin.F_joint; F_nest], ...
    [r6.p_joint; r6_curv.p_joint; r_lin.p_joint; p_nest], ...
    [p_boot_quad; NaN; p_boot_lin; NaN], ...
    'VariableNames', {'Test','F_stat','p_value','p_wild_cluster_bootstrap'});
 
disp('TABLE 6 TESTS');
disp(Table6_tests);
fprintf('Residual sum of squares: linear = %.1f, quadratic = %.1f\n', rss1, rss2);
 
writetable(Table6,       'Regression_Results.xlsx', 'Sheet', 'Table6_Coefficients');
writetable(Table6_tests, 'Regression_Results.xlsx', 'Sheet', 'Table6_Tests');
 
%% =====================================================================
% Figure: Fitted age profiles for the two cohorts using Table 6
% Round effects are held at their sample average. 95% CIs included
% (cluster-robust, t with G-1 df). Line drawn only over ages actually
% observed in a cohort.
% ======================================================================

b6      = r6.b;
V6      = r6.V;
fe_mean = mean(FE(:, 2:end), 1);          % average round effect (1 x 12)
fe_b    = b6(7:end);                      % round FE coefficients
S.LFPR_adj = S.est_pct - (FE(:, 2:end) - fe_mean) * fe_b;
 
mean_age = mean(S.Age);
tcrit    = tinv(0.975, r6.G - 1);
colours  = [0 0.447 0.741; 0.850 0.325 0.098];
labels   = {'1980-84 cohort', '1985-89 cohort'};
 
fig = figure('Color', 'w', 'Position', [100 100 820 520]);
hold on;
for c = 0:1
    col = colours(c+1, :);
    sel = (S.Younger == c);
    ages = (min(S.Age(sel)) : 0.25 : max(S.Age(sel)))';
    a    = ages - mean_age;
    d    = c * ones(size(a));
 
    x0 = [ones(size(a)), a, a.^2, d, a.*d, (a.^2).*d, ...
          repmat(fe_mean, numel(a), 1)];
    fitted = x0 * b6;
    se     = sqrt(sum((x0 * V6) .* x0, 2));
 
    fill([ages; flipud(ages)], ...
         [fitted - tcrit*se; flipud(fitted + tcrit*se)], col, ...
         'FaceAlpha', 0.15, 'EdgeColor', 'none', 'HandleVisibility', 'off');
    scatter(S.Age(sel), S.LFPR_adj(sel), 18, col, 'filled', ...
         'MarkerFaceAlpha', 0.45, 'HandleVisibility', 'off');
    plot(ages, fitted, '-', 'Color', col, 'LineWidth', 2.2, ...
         'DisplayName', labels{c+1});
end
hold off;
grid on; box on;
xlabel('Age');
ylabel('Female LFPR, UPSS (%)');
title({'Quadratic age profiles by birth cohort', ...
       '(round effects held at their average; shaded = 95% CI)'});
legend('Location', 'northwest', 'Box', 'off');
 
print(fig, 'Figure_Cohort_Age_Profiles.png', '-dpng', '-r300');
disp('Figure saved as Figure_Cohort_Age_Profiles.png');
disp('All tables exported successfully.');
 
%% ==========================================================
% Helper Functions
% ===========================================================
 
% (Weighted) OLS with cluster-robust (CR1) standard errors.
% jidx = indices of the jointly tested coefficients(F test, q and G-1 df).
function out = cluster_reg(y, X, w, cl, jidx)
    sw = sqrt(w);
    ys = y .* sw;
    Xs = X .* sw;
    [N, K] = size(Xs);
 
    XtXi = inv(Xs' * Xs);
    b    = XtXi * (Xs' * ys);
    e    = ys - Xs * b;
 
    ids  = unique(cl);
    G    = numel(ids);
    meat = zeros(K, K);
    for g = 1:G
        in = (cl == ids(g));
        s  = Xs(in, :)' * e(in);
        meat = meat + s * s';
    end
 
    V  = (G/(G-1)) * ((N-1)/(N-K)) * XtXi * meat * XtXi;
    se = sqrt(diag(V));
    t  = b ./ se;
    p  = 2 * tcdf(-abs(t), G-1);
 
    q  = numel(jidx);
    bj = b(jidx);
    Wj = bj' * (V(jidx, jidx) \ bj);
    Fj = Wj / q;
 
    out.b = b;   out.se = se;   out.t = t;   out.p = p;   out.V = V;
    out.G = G;
    out.W_joint = Wj;
    out.F_joint = Fj;
    out.p_joint = 1 - fcdf(Fj, q, G-1);
end
 
% Wild cluster bootstrap p-value for a joint Wald test (OLS, null imposed)
% Enumerates all 2^G Rademacher sign patterns, so there is no simulation noise
function [W0, p_boot] = wild_cluster_boot(y, X, cl, jidx)
    [N, K] = size(X);
    [~, ~, g] = unique(cl);
    G  = max(g);
 
    Dm   = sparse(1:N, g, 1, N, G);     % cluster membership matrix
    XtXi = inv(X' * X);
 
    W0 = joint_wald(y, X, XtXi, Dm, G, jidx);
 
    % Restricted model: drops the tested coefficients
    keep  = setdiff(1:K, jidx);
    Xr    = X(:, keep);
    fit_r = Xr * (Xr \ y);
    res   = y - fit_r;
 
    warning('off', 'MATLAB:singularMatrix');
    warning('off', 'MATLAB:nearlySingularMatrix');
 
    nsign = 2^G;
    Wstar = nan(nsign, 1);
    for s = 0:nsign-1
        signs = 2 * double(bitget(s, 1:G)') - 1;     % G x 1 of -1/+1
        ystar = fit_r + signs(g) .* res;
        Wstar(s+1) = joint_wald(ystar, X, XtXi, Dm, G, jidx);
    end
 
    warning('on', 'MATLAB:singularMatrix');
    warning('on', 'MATLAB:nearlySingularMatrix');
 
    valid  = isfinite(Wstar);
    p_boot = mean(Wstar(valid) >= W0 - 1e-9);
end
 
% Cluster-robust joint Wald statistic for fixed design X.
function W = joint_wald(y, X, XtXi, Dm, G, jidx)
    [N, K] = size(X);
    b  = XtXi * (X' * y);
    e  = y - X * b;
    Sg = Dm' * (X .* e);                 % G x K cluster score sums
    V  = (G/(G-1)) * ((N-1)/(N-K)) * XtXi * (Sg' * Sg) * XtXi;
    bj = b(jidx);
    W  = bj' * (V(jidx, jidx) \ bj);
end
