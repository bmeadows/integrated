FOR /L %%A IN (1,1,10) DO (
  swipl -s qRRL.pl --quiet -t runbatchdefault
)
