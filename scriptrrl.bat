FOR /L %%A IN (1,1,100) DO (
  swipl -s qRRL.pl --quiet -t runbatchdefault
)
