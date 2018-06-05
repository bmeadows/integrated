FOR /L %%A IN (1,1,6) DO (
  swipl -s qRRL.pl --quiet -t runbatchdefault
)
